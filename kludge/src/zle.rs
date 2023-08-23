use std::{
    collections::HashMap,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    process::exit,
};

use tracing::debug;

use crate::build;

mod extract;

use extract::Cmd;

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[command(subcommand)]
    pub(crate) cmd: Command,
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum Command {
    Expand {
        conf: PathBuf,
        lbuf: String,
        rbuf: String,
    },
    Extract {
        cmd: String,
        conf: Option<PathBuf>,
    },
    Hint {
        #[arg(long, default_value_t = u8::MAX)]
        max: u8,

        conf: PathBuf,
        buf: String,
    },
    Init,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub(crate) struct ConfigFile {
    #[serde(default)]
    cmds: Vec<Cmd>,
}

impl ConfigFile {
    fn from_file<P: AsRef<Path>>(p: P) -> Self {
        toml::from_str::<ConfigFile>(&fs::read_to_string(p).unwrap()).unwrap()
    }
}

const DETCK: &str = "cargo clippy --workspace --all-targets -- --deny warnings
cargo build --locked --all-targets --workspace
pushd dagx-libffi/test-data/c && make && popd
cargo test --locked --workspace --exclude demo1 --exclude dxezz
cargo test --locked --workspace -- demo1 dxezz --test-threads=1";

fn compile(cmds: Vec<Cmd>) -> HashMap<String, String> {
    let mut m = HashMap::with_capacity(cmds.len());
    let mut bind = |k: String, mut v: String| {
        debug!("binding '{k}' to '{v}'");
        if let Some(_existing) = m.get(k.as_str()) {
            // TODO
            // warn!("Map already contained key! {k} -> {existing}, {v}");
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

pub(crate) fn expand_pre(conf: ConfigFile, lbuf: String) -> Option<String> {
    let compiled = compile(conf.cmds);
    if let Some(r) = compiled.get(lbuf.as_str()) {
        debug!("Expanding {lbuf} to {r}");
        return Some(r.clone());
    }
    if lbuf == "ci" {
        let pwd = std::env::current_dir().ok()?;
        if pwd.file_name() == Some(OsStr::new("detect")) {
            Some(String::from(DETCK))
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

pub(crate) fn expand(conf: ConfigFile, mut lbuf: String, rbuf: String) -> Option<String> {
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
    let expanded = expand_pre(conf, lbuf);
    debug!("expanded = {expanded:?}");
    expanded.map(|s| format!("{prefix}{s}"))
}

pub(crate) fn go(conf: Config) {
    match conf.cmd {
        Command::Expand { conf, lbuf, rbuf } => {
            let conf = ConfigFile::from_file(conf);
            if let Some(result) = expand(conf, lbuf, rbuf) {
                println!("{}", result);
                exit(0);
            }
            exit(1)
        }
        Command::Extract { conf, cmd } => {
            let conf = if let Some(conf) = conf {
                extract::ConfigFile::from_file(conf)
            } else {
                extract::ConfigFile::default()
            };
            if let Some(cmd) = extract::extract(conf, cmd) {
                let conf = ConfigFile { cmds: vec![cmd] };
                println!("{}", toml::to_string(&conf).unwrap())
            }
        }
        Command::Hint { conf, buf, max } => {
            let conf = ConfigFile::from_file(conf);
            let mut compiled = compile(conf.cmds).into_iter().collect::<Vec<_>>();
            compiled.sort();
            let mut seen = 0;
            for (k, v) in compiled {
                if k.starts_with(&buf) {
                    seen += 1;
                    println!("{k} --> {v}");
                }
                if seen >= max {
                    break;
                }
            }
        }
        Command::Init => println!("{}", include_str!("init.zsh")),
    }
}
