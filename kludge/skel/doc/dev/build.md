# Build

To build and install from source, you'll need to install Rust and
[Cargo][cargo]. Follow the instructions on the [Rust installation
page][install-rust]. Then, get the source:

[cargo]: https://doc.rust-lang.org/cargo/
[install-rust]: https://www.rust-lang.org/tools/install

```bash
git clone https://github.com/langston-barrett/{{name}}
cd {{name}}
```

Finally, build everything:

```bash
cargo build --locked --release
```

This will put the binaries in `target/release`.

You can install the binary to `~/.cargo/bin` with:

```bash
cargo install --locked --path=.
```
