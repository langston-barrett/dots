# Directions for coding agents

## Code style

### Haskell

- Avoid defining functions by cases on their last argument, use `LambdaCase`
- Avoid list indexing (`!!`)
- Avoid partial functions (`error`, `head`, `undefined`) except in tests
- Instead of nesting `Either`, `Maybe`, and tuples, make a new `data` type
- `LambdaCase` now supports `\cases` for multiple arguments
- Never return tuples of arity higher than 2, just make a new `data` type
- Never use `unsafeCoerce` or `unsafePerformIO`
- Only use `Either` for error handling, otherwise make a new `data` type
- Use `Data.Text.IO` for I/O

### Python

- Use `dataclasses` where possible
- Use `pathlib` where possible

#### Python scripts

- Use `argparse` like so: `parser = ArgumentParser(description=__doc__)`
- Use the shebang `#!/usr/bin/env python3`
- Write a very concise one line module docstring

### Rust

- Use `&Path`, not `&PathBuf`
- Use `&str`, not `&String`
- When done, run `cargo fmt`
- When done, run `cargo clippy --allow-dirty --fix -- --deny warnings`

#### Rust applications

- Use `anyhow` and `.with_context` instead of `unwrap` or bare `?`

#### Rust libraries

- Implement `error::Error` for error types
- Return a `Result` with a dedicated error type from fallible functions

### Shell

- Do not unnecessarily capitalize variables
- Log to stderr, not stdout
- Use `--` before variable arguments (`$1`, `$@`, etc.)
- Use `printf` instead of `echo`
- Use `set -euo pipefail`
- Use `#!/usr/bin/env bash` as the shebang
