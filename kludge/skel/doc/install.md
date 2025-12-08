# Installation

## Pre-built binaries

### Via the browser

Navigate to the most recent release on the [releases page] and download the
desired artifact(s).

[releases page]: https://github.com/langston-barrett/{{name}}/releases

### Via `curl`

You can download binaries with `curl` like so. Replace `X.Y.Z` with
the most recent version number and `TARGET` with `x86_64-apple-darwin`,
`x86_64-unknown-linux-gnu`, or `x86_64-unknown-linux-musl`, and run:

```sh
curl \
  --fail \
  --location \
  --proto '=https' \
  --show-error \
  --silent \
  --tlsv1.2 \
  https://github.com/langston-barrett/{{name}}/releases/download/vX.Y.Z/{{name}}-TARGET.gz | \
  gunzip --to-stdout > {{name}}
```

## From source

To build from source, you'll need Rust and [Cargo][cargo]. Follow the
instructions on the [Rust installation page][install-rust].

[cargo]: https://doc.rust-lang.org/cargo/
[install-rust]: https://www.rust-lang.org/tools/install

### Via Cargo

You can build a released version from [crates.io] using Cargo. To install the
latest release, run:

[crates.io]: https://crates.io/

```sh
cargo install --locked {{name}}
```

Alternatively, you can install the latest, unreleased version with:

```sh
cargo install --locked --git https://github.com/langston-barrett/{{name}}.git {{name}}
```

### From a local checkout

See the [developer's guide](dev/build.md).
