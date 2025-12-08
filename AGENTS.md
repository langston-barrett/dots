# Directions for coding agents

## Code style

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
