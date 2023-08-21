use std::{collections::HashMap, ffi::OsStr, process::exit};

use tracing::debug;

use crate::build;

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[command(subcommand)]
    pub(crate) cmd: Command,
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum Command {
    Expand { lbuf: String, rbuf: String },
    Init,
}

const DETCK: &str = "cargo clippy --workspace --all-targets -- --deny warnings
cargo build --locked --all-targets --workspace
pushd dagx-libffi/test-data/c && make && popd
cargo test --locked --workspace --exclude demo1 --exclude dxezz
cargo test --locked --workspace -- demo1 dxezz --test-threads=1";

struct Cmd {
    short: String,
    long: String,
    flags: HashMap<String, String>,
    subs: Vec<Cmd>,
}

fn compile(cmds: Vec<Cmd>) -> HashMap<String, String> {
    let mut m = HashMap::with_capacity(cmds.len());
    let mut bind = |k: String, mut v: String| {
        debug!("binding '{k}' to '{v}'");
        if m.contains_key(k.as_str()) {
            panic!("Contained key! {k}");
        }
        if !v.ends_with(' ') {
            v = format!("{v} ");
        }
        m.insert(k, v);
    };
    for c in cmds {
        let short = c.short;
        let long = c.long;
        for (f, fl) in &c.flags {
            let expanded = format!("{long} {}", fl.clone());
            bind(format!("{short} {f}"), expanded.clone());
            bind(format!("{long} {f}"), expanded);
        }
        for (k, v) in compile(c.subs) {
            bind(format!("{short}{}", k), format!("{long} {v}"));
            bind(format!("{long} {}", k), format!("{long} {v}"));
        }
        bind(short, long);
    }
    m
}

pub(crate) fn expand_pre(lbuf: String) -> Option<String> {
    let cmds = vec![
        Cmd {
            short: String::from("cb"),
            long: String::from("cabal"),
            flags: HashMap::new(),
            subs: vec![
                Cmd {
                    short: String::from("b"),
                    long: String::from("build"),
                    flags: HashMap::new(),
                    subs: vec![],
                },
                Cmd {
                    short: String::from("r"),
                    long: String::from("run"),
                    flags: HashMap::new(),
                    subs: vec![],
                },
            ],
        },
        Cmd {
            short: String::from("cg"),
            long: String::from("cargo"),
            flags: HashMap::new(),
            subs: vec![],
        },
        Cmd {
            short: String::from("dk"),
            long: String::from("sudo -g docker docker"),
            flags: HashMap::new(),
            subs: vec![],
        },
        Cmd {
            short: String::from("g"),
            long: String::from("git"),
            flags: HashMap::new(),
            subs: vec![],
        },
    ];
    let compiled = compile(cmds);
    if let Some(r) = compiled.get(lbuf.as_str()) {
        return Some(r.clone());
    }
    if lbuf == "ci" {
        let pwd = std::env::current_dir().ok()?;
        if pwd.file_name() == Some(OsStr::new("detect")) {
            return Some(String::from(DETCK));
        } else {
            None
        }
    } else if lbuf == "g" {
        Some(String::from("git "))
    } else if lbuf == "gcm" || lbuf == "git cm" {
        Some(String::from("git commit "))
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
