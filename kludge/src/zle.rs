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

pub(crate) fn expand_pre(mut lbuf: String) -> Option<String> {
    if lbuf == "ci" {
        let pwd = std::env::current_dir().ok()?;
        if pwd.file_name() == Some(OsStr::new("detect")) {
            return Some(String::from(DETCK));
        } else {
            None
        }
    } else if lbuf == "cb" {
        Some(String::from("cabal "))
    } else if lbuf == "cg" {
        Some(String::from("cargo "))
    } else if lbuf == "dk" {
        Some(String::from("sudo -g docker docker "))
    } else if lbuf == "g" {
        Some(String::from("git "))
    } else if lbuf == "gsh" {
        Some(String::from("git stash "))
    } else if lbuf == "grb" || lbuf == "git rb" {
        Some(String::from("git rebase "))
    } else if lbuf == "grs" || lbuf == "git rs" {
        Some(String::from("git reset "))
    } else if lbuf == "grv" || lbuf == "git rv" {
        Some(String::from("git revert "))
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
    } else if lbuf == "w" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("ls ./**/*.cabal ./**/*.hs | entr -c -s 'cabal build'
ghcid --target=test:grease-tests")),
            Some(build::System::Cargo) => {
                Some(String::from("ls ./**/Cargo.toml ./**/*.rs | entr -c -s 'cargo fmt && cargo clippy -- --deny warnings'"))
            }
            Some(build::System::Make) => Some(String::from("make test")),
            None => None,
        }
    } else {
        None
    }
}

pub(crate) fn expand(mut lbuf: String, rbuf: String) -> Option<String> {
    if !rbuf.is_empty() {
        return None;
    }
    let mut prefix = String::new();
    for delim in [" || ", " && ", "; "] {
        if let Some(idx) = lbuf.find(delim) {
            let after = idx + delim.len();
            let (pre, post) = lbuf.split_at(after);
            prefix = String::from(pre);
            lbuf = String::from(post);
        }
    }
    expand_pre(lbuf).map(|s| format!("{prefix}{s}"))
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
