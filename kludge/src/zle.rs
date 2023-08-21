use std::{ffi::OsStr, process::exit};

use crate::build;

#[derive(clap::Parser)]
pub(crate) struct Config {
    #[command(subcommand)]
    pub(crate) cmd: Command,
}

#[derive(clap::Subcommand)]
pub(crate) enum Command {
    Expand { lbuf: String, rbuf: String },
    Init,
}

const DETCK: &str = "cargo clippy --workspace --all-targets -- --deny warnings
cargo build --locked --all-targets --workspace
pushd dagx-libffi/test-data/c && make && popd
cargo test --locked --workspace --exclude demo1 --exclude dxezz
cargo test --locked --workspace -- demo1 dxezz --test-threads=1";

pub(crate) fn expand(lbuf: String, rbuf: String) -> Option<String> {
    if !rbuf.is_empty() {
        None
    } else if lbuf == "ci" {
        let pwd = std::env::current_dir().ok()?;
        if pwd.file_name() == Some(OsStr::new("detect")) {
            return Some(String::from(DETCK));
        } else {
            None
        }
    } else if lbuf == "dk" {
        Some(String::from("sudo -g docker docker "))
    } else if lbuf == "gpt" {
        Some(String::from("chatgpt.sh --prompt '"))
    } else if lbuf == "t" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal test")),
            Some(build::System::Cargo) => Some(String::from("cargo test")),
            Some(build::System::Make) => Some(String::from("make test")),
            None => None,
        }
    } else {
        None
    }
}

pub(crate) fn go(conf: Config) {
    match conf.cmd {
        Command::Expand { lbuf, rbuf } => {
            if let Some(result) = expand(lbuf, rbuf) {
                println!("{}", result);
                exit(0);
            }
        }
        Command::Init => println!("{}", include_str!("init.zsh")),
    }
    exit(1)
}
