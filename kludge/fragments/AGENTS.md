# Directions for coding agents

## Code style

### Python

- Use `pathlib` where possible

#### Python scripts

- Use `argparse` like so: `parser = ArgumentParser(description=__doc__)`
- Use the shebang `#!/usr/bin/env python3`
- Write a very concise one line docstring

### Rust

- Use `&Path`, not `&PathBuf`
- Use `&str`, not `&String`

#### Rust applications

- Use `anyhow` and `.with_context` instead of `unwrap` or bare `?`

#### Rust libraries

- Return a `Result` with a dedicated error type from fallible functions
- Implement `error::Error` for error types

### Shell

- Use `#!/usr/bin/env bash` as the shebang
- Do not unnecessarily capitalize variables
