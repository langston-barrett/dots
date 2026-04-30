#!/usr/bin/env python3
"""Audit a Rust codebase for security vulnerabilities using parallel claude subagents."""

import argparse
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path

MAX_WORKERS = 10

# Each entry is (pattern, example) where example must match the pattern.
# Patterns are Perl-compatible regexes (git grep -P).
GREP_PATTERNS: list[tuple[str, str]] = [
    # panic-based DoS
    (r"\.unwrap\(\)", "result.unwrap()"),
    (r"\.expect\(", 'result.expect("msg")'),
    # silently discarded errors
    (r"let _ =", "let _ = foo();"),
    (r"\.ok\(\)", "result.ok()"),
    # filesystem path operations (TOCTOU, permissions windows)
    (r"std::fs::", "std::fs::rename(a, b)"),
    (r"File::create", 'File::create("x")'),
    (r"OpenOptions", "OpenOptions::new()"),
    (r"set_permissions", "fs::set_permissions(p, m)"),
    (r"canonicalize", "path.canonicalize()"),
    # path string comparisons — all four operator/operand-order combinations
    (r"path\s*==", 'if path == "/" {'),
    (r"==\s*path", 'if "/" == path {'),
    (r"path\s*!=", 'if path != "/" {'),
    (r"!=\s*path", 'if "/" != path {'),
    (r"==\s*Path::", 'if p == Path::new("/") {'),
    (r"!=\s*Path::", 'if p != Path::new("/") {'),
    (r"Path::new\b.*==", 'if Path::new("/") == p {'),
    (r"Path::new\b.*!=", 'if Path::new("/") != p {'),
    # UTF-8 / encoding assumptions
    (r"\.to_str\(\)", "osstr.to_str()"),
    (r"to_string_lossy", "osstr.to_string_lossy()"),
    (r"from_utf8", "str::from_utf8(&buf)"),
]


def _check_patterns() -> None:
    for pattern, example in GREP_PATTERNS:
        assert re.search(pattern, example), (
            f"Pattern {pattern!r} does not match its example {example!r}"
        )


def load_checklist() -> str:
    skill_md = Path(__file__).parent.parent / "SKILL.md"
    text = skill_md.read_text()
    cutoff = text.find("\n## Codebase-Wide Audit")
    return text[:cutoff].strip() if cutoff != -1 else text.strip()


def find_candidates() -> list[str]:
    args = ["git", "grep", "-Pl"]
    for pattern, _ in GREP_PATTERNS:
        args += ["-e", pattern]
    args += ["--", "*.rs"]
    result = subprocess.run(args, capture_output=True, text=True)
    if result.returncode not in (0, 1):
        print(result.stderr, file=sys.stderr)
        result.check_returncode()
    return [f for f in result.stdout.splitlines() if f]


@dataclass
class FileReport:
    filepath: str
    text: str


def audit_file(filepath: str, checklist: str) -> FileReport:
    prompt = (
        f"Audit `{filepath}` for security vulnerabilities. Read the file, then deeply "
        "investigate any suspicious patterns: look up the definitions of functions it "
        "calls, trace how values from untrusted sources flow through the code, and check "
        "how callers use the APIs it exposes. Only report bugs you are *certain* of after "
        "this investigation. For each finding report: file path, line number, category, "
        "concrete risk, and minimal fix. If you find no bugs, say so explicitly.\n\n"
        f"Checklist:\n{checklist}"
    )
    result = subprocess.run(
        ["claude", "-p", prompt, "--output-format", "text"],
        capture_output=True,
        text=True,
        check=True,
    )
    return FileReport(filepath=filepath, text=result.stdout.strip())


def triage(reports: list[FileReport]) -> str:
    combined = "\n\n".join(f"=== {r.filepath} ===\n{r.text}" for r in reports)
    prompt = (
        "Review the following per-file security audit reports for a Rust codebase. "
        "For each finding, confirm it as a real bug or discard it as a false positive, "
        "with a one-line justification. Deduplicate any findings that refer to the same "
        f"root cause across files.\n\n{combined}"
    )
    result = subprocess.run(
        ["claude", "-p", prompt, "--output-format", "text"],
        capture_output=True,
        text=True,
        check=True,
    )
    return result.stdout.strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.parse_args()

    _check_patterns()
    checklist = load_checklist()

    print("Finding candidate files...", flush=True)
    candidates = find_candidates()
    if not candidates:
        print("No candidate files found.")
        return

    print(f"Auditing {len(candidates)} file(s) in parallel:", flush=True)
    for f in candidates:
        print(f"  {f}")
    print(flush=True)

    reports: list[FileReport] = []
    with ThreadPoolExecutor(max_workers=min(len(candidates), MAX_WORKERS)) as pool:
        futures = {pool.submit(audit_file, f, checklist): f for f in candidates}
        for future in as_completed(futures):
            filepath = futures[future]
            try:
                report = future.result()
                reports.append(report)
                print(f"[done] {filepath}", flush=True)
            except Exception as e:
                print(f"[error] {filepath}: {e}", file=sys.stderr)

    findings = [r for r in reports if "no bugs" not in r.text.lower()]
    if not findings:
        print("\nNo findings reported.")
        return

    print(f"\nTriaging {len(findings)} report(s)...\n", flush=True)
    print(triage(findings))


if __name__ == "__main__":
    main()
