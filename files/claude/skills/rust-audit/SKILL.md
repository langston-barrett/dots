---
name: rust-audit
description: Security audit checklist for Rust code. Use when auditing Rust code for security vulnerabilities.
---

Run `scripts/audit.py` (located relative to this skill file) to perform the audit. Do not audit files manually — the script handles finding candidates, spawning subagents, and triaging results.

Only report bugs that you are *certain* of.

## TOCTOU (Time-of-Check-Time-of-Use)

- Are there path-based operations split across multiple syscalls (e.g., `exists()` then `open()`, `metadata()` then `rename()`)?
- Could an attacker replace a path with a symlink between a check and an action?
- Prefer operating on an open `File` handle (std): `file.metadata()`, `file.set_permissions()`, etc. don't re-resolve the path and are safe from symlink swaps.
- For fd-anchored cross-file operations (`openat`, `renameat`, `linkat`), use the `rustix` or `nix` crates — these aren't in std.
- Use `symlink_metadata()` (std) or `O_NOFOLLOW` via `rustix`/`nix` when you don't want to follow symlinks.
- The ergonomic stdlib APIs (`std::fs::rename`, `std::fs::copy`, etc.) re-resolve paths on each call — flag their use in security-sensitive paths.

## Permissions Windows

- Are files created with default permissions and then `chmod`-ed afterward? This creates a window where the file is world-readable.
- Use `OpenOptions::mode()` (Unix) or equivalent to set permissions atomically at creation time.
- Check for `File::create()` followed by a separate `set_permissions()` call.

## Path Comparison Pitfalls

- Are paths compared as strings (e.g., `path == "/"`)? Alternate representations like `/../` can bypass these checks.
- All path comparisons should use `fs::canonicalize()` or equivalent before comparing.
- Flag `==` comparisons on `Path`, `PathBuf`, or string literals that look like filesystem paths.

## UTF-8 / Encoding Assumptions

- Are `OsStr`/`OsString` values forced through `.to_str()` or `.to_string_lossy()` without justification? This can silently corrupt non-UTF-8 filenames or byte streams.
- Use `OsStr`, `PathBuf`, and `&[u8]` for filesystem paths and binary streams; only convert to `String` when the data is guaranteed to be valid UTF-8.
- Flag `.unwrap()` on `.to_str()` calls — these panic on non-UTF-8 OS strings.

## Panic-Based Denial of Service

- Are there `unwrap()` or `expect()` calls on values derived from untrusted input (user-supplied arguments, file contents, network data)?
- Panics in long-running processes or system tools can cause outages.
- Prefer `?` propagation or explicit error handling over `unwrap()`/`expect()` at trust boundaries.

## Silently Discarded Errors

- Are `Result` values being ignored (implicit `let _ = ...` or missing `?`)?
- Enable `#[must_use]` lints and `clippy::must_use_candidate`.
- Check for `.ok()` calls that discard errors without comment.

## Behavioral Divergence (for reimplementations)

- Does the tool's behavior match the original in edge cases (empty input, special characters, large files, unusual flags)?
- Divergence can silently break dependent scripts that rely on specific exit codes, output format, or error messages.
- Run the original tool's test suite against the reimplementation.

