#!/usr/bin/env python3
"""Run cabal test suite and record timing for each test.

Usage:
    test-timings.py <package>

Examples:
    test-timings.py pkg:my-package-exe
    test-timings.py my-package-exe

Outputs:
    - Runs cabal test with --test-show-details=direct
    - Parses output to extract test names and timings
    - Prints summary sorted by time (slowest first)
    - Saves detailed results to test-timings-<timestamp>.txt
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
class TestResult:
    """Result of a single test."""

    name: str
    path: List[str]  # Hierarchical path (groups/subgroups)
    status: str  # OK, FAIL, etc.
    time: Optional[float]  # seconds, None if not provided
    occurrence: int  # For handling duplicate names


def parse_test_output(output: str) -> List[TestResult]:
    """Parse cabal test output and extract test results.

    Format:
        Tests
          Group name
            test-name:                          OK (0.05s)
            subgroup
              nested-test:                      OK (0.03s)

    Returns list of TestResult objects.
    """
    results = []
    path_stack: List[tuple[int, str]] = []  # [(indent_level, group_name), ...]
    test_counts: Dict[str, int] = {}  # Track occurrences of each test name

    for line in output.split("\n"):
        if not line.strip():
            continue

        # Skip configuration/build messages
        if any(
            skip in line
            for skip in [
                "Configuration is affected",
                "cabal.project",
                "Build profile:",
                "In order, the following",
                "Preprocessing",
                "Building",
                "Running",
                "Test suite",
                "Test suite logged to",
                "Package coverage report",
                "Writing:",
                "of 1 test",
            ]
        ):
            continue

        # Calculate indentation level (spaces at start)
        indent = len(line) - len(line.lstrip(" "))
        stripped = line.strip()

        # Check if this is a test result line (has ':' followed by status)
        # Format: "test-name:    OK (0.05s)" or "test-name:    OK"
        match = re.match(
            r"^(.+?):\s+(OK|FAIL|SKIPPED?)\s*(?:\(([0-9.]+)s\))?", stripped
        )

        if match:
            test_name = match.group(1).strip()
            status = match.group(2)
            time_str = match.group(3)
            time_val = float(time_str) if time_str else None

            # Update path stack - remove groups at same or deeper level
            while path_stack and path_stack[-1][0] >= indent:
                path_stack.pop()

            # Build full path from stack
            current_path = [name for _, name in path_stack]

            # Track occurrence for duplicate names
            full_name = "/".join(current_path + [test_name])
            test_counts[full_name] = test_counts.get(full_name, 0) + 1

            results.append(
                TestResult(
                    name=test_name,
                    path=current_path.copy(),
                    status=status,
                    time=time_val,
                    occurrence=test_counts[full_name],
                )
            )
        else:
            # This might be a group name - add to path stack if it looks like one
            # Group names are indented and don't have ':'
            if stripped and not stripped.startswith("✓") and ":" not in stripped:
                # Check if this looks like a summary line
                if (
                    "passed" in stripped
                    or "failed" in stripped
                    or "tests" in stripped.lower()
                ):
                    continue

                # Update path stack
                while path_stack and path_stack[-1][0] >= indent:
                    path_stack.pop()

                path_stack.append((indent, stripped))

    return results


def format_time(seconds: Optional[float]) -> str:
    """Format time in seconds to a readable string."""
    if seconds is None:
        return "     -"
    if seconds < 0.01:
        return f"{seconds:6.3f}s"
    if seconds < 1:
        return f"{seconds:6.2f}s"
    return f"{seconds:6.2f}s"


def format_test_name(result: TestResult, max_width: int = 60) -> str:
    """Format test name with its path."""
    if result.path:
        full_name = "/".join(result.path) + "/" + result.name
    else:
        full_name = result.name

    # Add occurrence number if > 1
    if result.occurrence > 1:
        full_name += f" [{result.occurrence}]"

    # Truncate if too long
    if len(full_name) > max_width:
        full_name = "..." + full_name[-(max_width - 3) :]

    return full_name


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "package", help="Package to test (e.g., my-package-exe or pkg:my-package-exe)"
    )
    args = parser.parse_args()

    package = args.package
    if not package.startswith("pkg:"):
        package = f"pkg:{package}"

    print(f"Running test suite: {package}")
    print("This may take a while...")
    print()

    # Run cabal test
    start_time = datetime.now()
    test_result = subprocess.run(
        ["cabal", "test", package, "--test-show-details=direct"],
        capture_output=True,
        text=True,
    )
    end_time = datetime.now()
    elapsed = (end_time - start_time).total_seconds()

    # Combine stdout and stderr
    output = test_result.stdout + test_result.stderr

    # Parse test results
    results = parse_test_output(output)

    if not results:
        print("Error: Could not parse test results")
        print()
        print("Output:")
        print(output)
        sys.exit(1)

    # Calculate statistics
    total_tests = len(results)
    passed = sum(1 for r in results if r.status == "OK")
    failed = sum(1 for r in results if r.status in ["FAIL", "FAILED"])
    timed_tests = [r for r in results if r.time is not None]
    total_test_time = sum((r.time for r in timed_tests if r.time is not None), 0.0)

    # Sort by time (slowest first), then by name
    sorted_results = sorted(
        results,
        key=lambda r: (
            r.time if r.time is not None else -1,
            "/".join(r.path + [r.name]),
        ),
        reverse=True,
    )

    # Create output directory and file
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_file = Path(f"test-timings-{timestamp}.txt")

    # Print and save summary
    def print_and_log(msg: str = ""):
        print(msg)
        with open(output_file, "a") as f:
            f.write(msg + "\n")

    # Clear output file
    output_file.write_text("")

    print_and_log("=" * 80)
    print_and_log(f"Test Suite Results: {package}")
    print_and_log("=" * 80)
    print_and_log()
    print_and_log(f"Total tests:     {total_tests}")
    print_and_log(f"Passed:          {passed}")
    print_and_log(f"Failed:          {failed}")
    print_and_log(f"Tests with time: {len(timed_tests)}")
    print_and_log(f"Total test time: {total_test_time:.2f}s")
    print_and_log(f"Wall clock time: {elapsed:.2f}s")
    print_and_log()

    # Print slowest tests (top 20)
    print_and_log("Slowest Tests:")
    print_and_log("-" * 80)

    slowest = [r for r in sorted_results if r.time is not None][:20]
    for result in slowest:
        time_str = format_time(result.time)
        name_str = format_test_name(result, max_width=70)
        print_and_log(f"{time_str}  {name_str}")

    print_and_log()

    # Print all tests
    print_and_log("All Tests (sorted by time):")
    print_and_log("-" * 80)

    for result in sorted_results:
        time_str = format_time(result.time)
        status_str = result.status.ljust(6)
        name_str = format_test_name(result, max_width=65)
        print_and_log(f"{time_str}  {status_str}  {name_str}")

    print()
    print(f"Detailed results saved to: {output_file}")

    # Exit with test suite exit code
    sys.exit(test_result.returncode)


if __name__ == "__main__":
    main()
