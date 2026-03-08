#!/usr/bin/env python3
"""Profile Haskell executables with multiple profiling methods and analyze results.

Usage:
    profile.py [--with-valgrind] <package> <program-arguments>

Examples:
    profile.py my-package-exe arg1 arg2
    profile.py --with-valgrind pkg:my-package-exe --flag value
"""

import argparse
import re
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional


@dataclass
class HPStats:
    """Statistics extracted from heap profile."""

    samples: int
    peak_bytes: int
    peak_time: float
    cost_centers: Dict[str, int]  # name -> total bytes


def parse_hp_file(hp_path: Path) -> Optional[HPStats]:
    """Parse a .hp heap profile file and extract statistics."""
    if not hp_path.exists():
        return None

    try:
        with open(hp_path) as f:
            lines = [line.strip() for line in f]

        samples = []
        cost_center_totals: Dict[str, int] = {}

        i = 0
        while i < len(lines):
            line = lines[i]

            # Look for BEGIN_SAMPLE
            if line.startswith("BEGIN_SAMPLE"):
                # Extract timestamp
                match = re.match(r"BEGIN_SAMPLE\s+([\d.]+)", line)
                timestamp = float(match.group(1)) if match else 0.0

                sample_data = {}
                i += 1

                # Read cost center data until END_SAMPLE
                while i < len(lines) and not lines[i].startswith("END_SAMPLE"):
                    # Parse "(id)cost_center_name\tbytes" format
                    parts = lines[i].split("\t")
                    if len(parts) >= 2:
                        cc_full_name = parts[0]
                        # Extract the cost center name (remove ID prefix like "(8027)")
                        cc_name = re.sub(r"^\(\d+\)", "", cc_full_name)
                        try:
                            bytes_val = int(parts[1])
                            sample_data[cc_name] = (
                                sample_data.get(cc_name, 0) + bytes_val
                            )
                            cost_center_totals[cc_name] = (
                                cost_center_totals.get(cc_name, 0) + bytes_val
                            )
                        except ValueError:
                            pass
                    i += 1

                samples.append((timestamp, sample_data))

            i += 1

        if not samples:
            return None

        # Find peak heap usage
        peak_bytes = 0
        peak_time = 0.0
        for timestamp, sample_data in samples:
            total_bytes = sum(sample_data.values())
            if total_bytes > peak_bytes:
                peak_bytes = total_bytes
                peak_time = timestamp

        return HPStats(
            samples=len(samples),
            peak_bytes=peak_bytes,
            peak_time=peak_time,
            cost_centers=cost_center_totals,
        )

    except Exception as e:
        print(f"Warning: Failed to parse {hp_path}: {e}", file=sys.stderr)
        return None


def log(log_file: Path, message: str):
    """Append message to log file."""
    with open(log_file, "a") as f:
        f.write(message + "\n")


def run_command(
    cmd: List[str],
    output_file: Path,
    log_file: Optional[Path] = None,
    stderr_file: Optional[Path] = None,
    check: bool = True,
) -> subprocess.CompletedProcess:
    """Run a command and capture output to files."""
    stderr_target = subprocess.STDOUT
    if stderr_file:
        stderr_target = open(stderr_file, "w")

    try:
        result = subprocess.run(
            cmd, stdout=open(output_file, "w"), stderr=stderr_target, check=check
        )
        return result
    finally:
        if stderr_file and stderr_target != subprocess.STDOUT:
            stderr_target.close()


def extract_rts_stats(stats_file: Path) -> Dict[str, str]:
    """Extract key metrics from RTS statistics output."""
    stats = {}

    if not stats_file.exists():
        return stats

    with open(stats_file) as f:
        content = f.read()

    # Extract metrics using regex
    patterns = {
        "bytes_alloc": r"([\d,]+)\s+bytes allocated in the heap",
        "bytes_copied": r"([\d,]+)\s+bytes copied during GC",
        "max_residency": r"([\d,]+)\s+bytes maximum residency",
    }

    for key, pattern in patterns.items():
        match = re.search(pattern, content)
        if match:
            value = match.group(1).replace(",", "")
            stats[key] = value

    return stats


def extract_valgrind_insns(callgrind_file: Path) -> Optional[str]:
    """Extract instruction count from callgrind output."""
    if not callgrind_file.exists():
        return None

    with open(callgrind_file) as f:
        for line in f:
            if line.startswith("summary:"):
                parts = line.split()
                if len(parts) >= 2:
                    return parts[1]

    return None


@dataclass
class PerfStats:
    """Statistics extracted from perf output."""

    instructions: str
    cycles: str
    ipc: Optional[str]
    cache_misses: Optional[str]
    cache_refs: Optional[str]
    branch_misses: Optional[str]
    branches: Optional[str]
    supported: bool


def extract_perf_stats(perf_file: Path) -> PerfStats:
    """Extract performance counters from perf output.

    Returns:
        PerfStats with instructions, cycles, cache misses, branch misses, etc.
    """
    if not perf_file.exists():
        return PerfStats(
            instructions="<not supported>",
            cycles="<not supported>",
            ipc=None,
            cache_misses=None,
            cache_refs=None,
            branch_misses=None,
            branches=None,
            supported=False,
        )

    with open(perf_file) as f:
        content = f.read()

    if "<not supported>" in content:
        return PerfStats(
            instructions="<not supported>",
            cycles="<not supported>",
            ipc=None,
            cache_misses=None,
            cache_refs=None,
            branch_misses=None,
            branches=None,
            supported=False,
        )

    # Parse perf output format: "  123,456  instructions"
    stats = {}
    patterns = {
        "instructions": r"([\d,]+)\s+instructions",
        "cycles": r"([\d,]+)\s+cycles",
        "cache-misses": r"([\d,]+)\s+cache-misses",
        "cache-references": r"([\d,]+)\s+cache-references",
        "branch-misses": r"([\d,]+)\s+branch-misses",
        "branches": r"([\d,]+)\s+branches",
    }

    for key, pattern in patterns.items():
        for line in content.split("\n"):
            if key in line and "<not" not in line:
                match = re.search(pattern, line)
                if match:
                    stats[key] = match.group(1).replace(",", "")
                    break

    # Calculate IPC
    ipc = None
    if "instructions" in stats and "cycles" in stats:
        try:
            insns_val = float(stats["instructions"])
            cycles_val = float(stats["cycles"])
            if insns_val > 0 and cycles_val > 0:
                ipc = f"{insns_val / cycles_val:.3f}"
        except ValueError:
            pass

    return PerfStats(
        instructions=stats.get("instructions", "<not supported>"),
        cycles=stats.get("cycles", "<not supported>"),
        ipc=ipc,
        cache_misses=stats.get("cache-misses"),
        cache_refs=stats.get("cache-references"),
        branch_misses=stats.get("branch-misses"),
        branches=stats.get("branches"),
        supported=bool(stats),
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "package",
        help="Package to profile (e.g., my-package-exe or pkg:my-package-exe)",
    )
    parser.add_argument(
        "--with-valgrind",
        action="store_true",
        help="Include valgrind instruction counting (slow)",
    )
    parser.add_argument(
        "prog_args", nargs="+", metavar="arg", help="Arguments to pass to the program"
    )
    args = parser.parse_args()

    package = args.package
    if not package.startswith("pkg:"):
        package = f"pkg:{package}"

    skip_valgrind = not args.with_valgrind
    prog_args: List[str] = args.prog_args

    # Extract base name for profiling output files (e.g., "my-package-exe" -> "my-package")
    base_name = package.replace("pkg:", "").replace("-exe", "")

    # Create output directory with timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = Path(f"profile-results-{timestamp}")
    output_dir.mkdir(exist_ok=True)

    log_file = output_dir / "run.log"

    print(f"Profiling output: {output_dir}/")
    print(f"Progress log: {log_file}")
    print()

    log(log_file, "=== Haskell Profiling Suite ===")
    log(log_file, f"Package: {package}")
    log(log_file, f"Arguments: {' '.join(prog_args)}")
    log(log_file, "")

    # Build package
    num_steps = 5 if skip_valgrind else 6
    log(log_file, f"[1/{num_steps}] Building {package}...")
    build_log = output_dir / "build.log"
    subprocess.run(
        ["cabal", "build", package],
        stdout=open(build_log, "w"),
        stderr=subprocess.STDOUT,
        check=True,
    )

    # Get binary path
    result = subprocess.run(
        ["cabal", "list-bin", package], capture_output=True, text=True, check=True
    )

    # Filter out configuration warnings
    binary_path: Optional[str] = None
    for line in result.stdout.split("\n"):
        if "Configuration is affected" in line or "cabal.project" in line:
            continue
        line = line.strip()
        if line and not line.startswith("Warning"):
            binary_path = line
            break

    if not binary_path:
        print(f"Error: Could not find {package} binary path", file=sys.stderr)
        sys.exit(1)

    # Type checker: binary_path is definitely str from here
    assert binary_path is not None

    log(log_file, f"Built: {binary_path}")

    # Clean up stale profiling files
    for filename in [f"{base_name}.tix", f"{base_name}.prof", f"{base_name}.hp"]:
        try:
            Path(filename).unlink()
        except FileNotFoundError:
            pass

    # 1. Baseline run with +RTS -s (runtime statistics)
    step = 2
    log(log_file, f"[{step}/{num_steps}] Running with RTS statistics (+RTS -s)...")
    run_command(
        [binary_path, "+RTS", "-s", "-RTS"] + prog_args,
        output_dir / "rts-output.log",
        stderr_file=output_dir / "rts-stats.log",
    )
    log(log_file, "Completed RTS run")

    # Extract RTS metrics
    rts_stats = extract_rts_stats(output_dir / "rts-stats.log")
    step += 1

    # 2. Valgrind callgrind (instruction counts) - OPTIONAL
    valgrind_insns = None
    if not skip_valgrind:
        log(
            log_file,
            f"[{step}/{num_steps}] Running with valgrind (instruction counts)...",
        )
        run_command(
            [
                "valgrind",
                "--tool=callgrind",
                f"--callgrind-out-file={output_dir / 'callgrind.out'}",
                f"--log-file={output_dir / 'valgrind.log'}",
                binary_path,
            ]
            + prog_args,
            output_dir / "valgrind-output.log",
        )
        log(log_file, "Completed valgrind run")
        valgrind_insns = extract_valgrind_insns(output_dir / "callgrind.out")
        step += 1

    # 3. Perf stat (performance counters)
    log(log_file, f"[{step}/{num_steps}] Running with perf (performance counters)...")
    # Collect comprehensive performance metrics
    perf_events = (
        "instructions,cycles,cache-misses,cache-references,branch-misses,branches"
    )
    run_command(
        [
            "perf",
            "stat",
            "-e",
            perf_events,
            "-o",
            str(output_dir / "perf-stats.log"),
            binary_path,
        ]
        + prog_args,
        output_dir / "perf-output.log",
        check=False,  # perf might not be supported
    )
    log(log_file, "Completed perf run")

    perf_stats = extract_perf_stats(output_dir / "perf-stats.log")
    step += 1

    # 4. Heap profiling (+RTS -hc -p)
    log(log_file, f"[{step}/{num_steps}] Running with heap profiling (+RTS -hc -p)...")

    # Clean up before heap profiling
    for filename in [f"{base_name}.tix", f"{base_name}.prof", f"{base_name}.hp"]:
        try:
            Path(filename).unlink()
        except FileNotFoundError:
            pass

    run_command(
        [binary_path, "+RTS", "-hc", "-p", "-s", "-RTS"] + prog_args,
        output_dir / "heap-output.log",
        stderr_file=output_dir / "heap-stats.log",
    )

    hp_file = Path(f"{base_name}.hp")
    if hp_file.exists():
        hp_file.rename(output_dir / f"{base_name}.hp")
        log(log_file, f"Generated {base_name}.hp")

    log(log_file, "Completed heap profiling run")
    step += 1

    # 5. Cost-center profiling (+RTS -p)
    log(
        log_file,
        f"[{step}/{num_steps}] Running with cost-center profiling (+RTS -p -s)...",
    )

    # Clean up before cost-center profiling
    for filename in [f"{base_name}.tix", f"{base_name}.prof"]:
        try:
            Path(filename).unlink()
        except FileNotFoundError:
            pass

    run_command(
        [binary_path, "+RTS", "-p", "-s", "-RTS"] + prog_args,
        output_dir / "prof-output.log",
        stderr_file=output_dir / "prof-stats.log",
    )

    prof_file = Path(f"{base_name}.prof")
    if prof_file.exists():
        prof_file.rename(output_dir / f"{base_name}.prof")
        log(log_file, f"Generated {base_name}.prof")

    log(log_file, "Completed cost-center profiling run")
    log(log_file, "")
    log(log_file, "=== All profiling runs complete ===")

    # Parse heap profile
    hp_stats = parse_hp_file(output_dir / f"{base_name}.hp")

    # Output summary to stdout
    print("=== Profiling Results ===")
    print()
    print("RTS Statistics:")
    print(f"  Bytes allocated:  {rts_stats.get('bytes_alloc', 'N/A')}")
    print(f"  Bytes copied (GC): {rts_stats.get('bytes_copied', 'N/A')}")
    print(f"  Max residency:    {rts_stats.get('max_residency', 'N/A')} bytes")
    print()

    if perf_stats.supported:
        print("Performance Counters (perf):")
        print(f"  Instructions:     {perf_stats.instructions}")
        print(f"  Cycles:           {perf_stats.cycles}")
        if perf_stats.ipc:
            print(f"  IPC:              {perf_stats.ipc}")

        if perf_stats.cache_refs:
            cache_miss_rate = None
            if perf_stats.cache_misses:
                try:
                    refs = float(perf_stats.cache_refs)
                    misses = float(perf_stats.cache_misses)
                    if refs > 0:
                        cache_miss_rate = f"{(misses / refs * 100):.2f}%"
                except ValueError:
                    pass
            print(f"  Cache references: {perf_stats.cache_refs}")
            print(f"  Cache misses:     {perf_stats.cache_misses or 'N/A'}", end="")
            if cache_miss_rate:
                print(f" ({cache_miss_rate})")
            else:
                print()

        if perf_stats.branches:
            branch_miss_rate = None
            if perf_stats.branch_misses:
                try:
                    branches = float(perf_stats.branches)
                    misses = float(perf_stats.branch_misses)
                    if branches > 0:
                        branch_miss_rate = f"{(misses / branches * 100):.2f}%"
                except ValueError:
                    pass
            print(f"  Branches:         {perf_stats.branches}")
            print(f"  Branch misses:    {perf_stats.branch_misses or 'N/A'}", end="")
            if branch_miss_rate:
                print(f" ({branch_miss_rate})")
            else:
                print()
    else:
        print("Performance Counters: <not supported on this system>")

    if valgrind_insns:
        print()
        print(f"Instructions (valgrind): {valgrind_insns}")

    print()

    # Print heap profile statistics
    if hp_stats:
        print("Heap Profile:")
        print(f"  Samples:         {hp_stats.samples}")
        print(f"  Peak heap:       {hp_stats.peak_bytes:,} bytes")
        print(f"  Peak at:         {hp_stats.peak_time:.2f}s")

        # Sort cost centers by total bytes and show top 5
        if hp_stats.cost_centers:
            print("  Top cost centers:")
            sorted_ccs = sorted(
                hp_stats.cost_centers.items(), key=lambda x: x[1], reverse=True
            )

            total_bytes = sum(hp_stats.cost_centers.values())

            for cc_name, cc_bytes in sorted_ccs[:5]:
                percentage = (cc_bytes / total_bytes * 100) if total_bytes > 0 else 0
                print(f"    {cc_name}: {cc_bytes:,} bytes ({percentage:.1f}%)")

        print()

    print("Files:")
    print(f"  Cost centers:   {output_dir / f'{base_name}.prof'}")
    print(f"  Heap profile:   {output_dir / f'{base_name}.hp'}")
    if not skip_valgrind:
        print(f"  Callgrind:      {output_dir / 'callgrind.out'}")


if __name__ == "__main__":
    main()
