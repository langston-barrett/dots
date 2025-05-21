use std::{env, error::Error, ffi::OsStr};

use crate::system as build;

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[clap(long)]
    aliases: bool,
    #[clap(long)]
    hint: bool,
    lbuf: String,
    rbuf: String,
}

fn expand_build_system(lbuf: &str) -> Option<String> {
    if lbuf == "b" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal build all")),
            Some(build::System::Cargo) => Some(String::from("cargo build ")),
            Some(build::System::Make) => Some(String::from("make ")),
            None => None,
        }
    } else if lbuf == "d" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal haddock ")),
            _ => None,
        }
    } else if lbuf == "i" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cargo) => Some(String::from("cargo install ")),
            _ => None,
        }
    } else if lbuf == "r" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal run ")),
            Some(build::System::Cargo) => Some(String::from("cargo run ")),
            Some(build::System::Make) => None,
            None => None,
        }
    } else if lbuf == "t" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal test ")),
            Some(build::System::Cargo) => Some(String::from("cargo test ")),
            Some(build::System::Make) => Some(String::from("make test ")),
            None => None,
        }
    } else if lbuf == "w" {
        let pwd = std::env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("ghcid")),
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

const ANYWHERE: &[(&str, &str)] = &[
    ("cb", "cabal"),
    ("cg", "cargo"),
    ("dk", "docker"),
    ("e", "hx"),
    ("m", "make"),
    ("nb", "nix-build"),
    ("nba", "nix-build -A"),
    ("ns", "nix-shell"),
    ("nsr", "nix-shell --run"),
    ("nsrzsh", "nix-shell --run 'exec zsh'"),
    ("py3", "python3"),
    ("rgall", "rg --hidden --no-ignore"),
    ("rmrf", "\rm -rf"),
    ("tp", "trash put"),
    (
        "bc",
        "clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0",
    ),
    (
        "ll",
        "clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -S -O0",
    ),
    ("sky", "ssh sky"),
    ("y", "clipboard"),
];

fn expand_anywhere(lbuf: &str, rbuf: &str) -> Option<String> {
    for (l, r) in ANYWHERE {
        if lbuf == *l && rbuf == "" {
            return Some(r.to_string());
        }
    }
    None
}

const BASIC: &[(&str, &[(&str, &str)])] = &[("detect", &[
    ("bs", "echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid && sudo sysctl kernel.perf_event_mlock_kb=2048 && cargo b -q --profile=profiling --bin=sofuzz && samply record ./target/profiling/sofuzz --solutions /run/user/1000/sols --gas=2048 sofuzz/rs/map/map.toml target/profiling/libsofuzz_map.so --no-check-dwarf"),
    ("clippy", "cargo clippy --all-targets -- --deny warnings"),
    ("e1", "rm -rf benign solutions ; cargo build -p=eval1-smi-model && cargo run --bin dxezz -- --qcow=targets/eval1-smi/image-debug/snapshots.qcow2 targets/eval1-smi/eval1-smi-debug.toml target/debug/libeval1_smi_model.so --seed=1 --outer-iterations=8 --inner-iterations=1 --no-check-snapshots -v"),
    ("rb", "cargo run --bin=bzro --"),
    ("rd", "cargo run --bin=dxezz --"),
    ("rs", "cargo run --bin=sofuzz --"),
    ("t", "cargo test"),
    ("tb", "cargo test --package=bzro -- --test-threads=1"),
    ("td", "cargo test --package=dxezz -- --test-threads=1"),
    ("ts", "cargo b -q --package=sofuzz-boxcar && cargo b -q --package=sofuzz-map && cargo test --package=sofuzz"),
]), ("grease", &[
    ("r", "cabal run exe:grease --"),
    ("t", "cabal run test:grease-tests --"),
    // TODO
    ("to", "cabal run exe:grease -- --symbol test $(fd --type=x elf tests/ | zshfzf)"),
    ("w", "ghcid"),
    ("wt", "ghcid --target=test:grease-tests"),
])];

fn expand_basic(lbuf: &str, rbuf: &str) -> Option<String> {
    let cwd = env::current_dir().ok()?;
    for (d, expands) in BASIC {
        let name = cwd.as_path().file_name().and_then(OsStr::to_str);
        if name == Some(d) {
            for (l, r) in *expands {
                if lbuf == *l && rbuf == "" {
                    return Some(r.to_string());
                }
            }
        }
    }
    None
}

fn expand(lbuf: String, rbuf: String) -> Option<String> {
    expand_build_system(&lbuf)
        .or_else(|| expand_anywhere(&lbuf, &rbuf))
        .or_else(|| expand_basic(&lbuf, &rbuf))
}

// TODO: Deduplicate logic
fn hint(lbuf: String, rbuf: String) -> Vec<(&'static str, &'static str)> {
    let mut results = Vec::with_capacity(8);
    for (l, r) in ANYWHERE {
        if l.starts_with(lbuf.as_str()) && rbuf == "" {
            results.push((*l, *r));
        }
    }
    if let Ok(cwd) = env::current_dir() {
        let name = cwd.as_path().file_name().and_then(OsStr::to_str);
        for (d, expands) in BASIC {
            if name == Some(d) {
                for (l, r) in *expands {
                    if l.starts_with(lbuf.as_str()) && rbuf == "" {
                        results.push((*l, *r));
                    }
                }
            }
        }
    }
    results
}

pub(super) fn go(conf: Config) -> Result<(), Box<dyn Error>> {
    // TODO: Help system
    if conf.aliases {
        for (l, r) in ANYWHERE {
            println!("alias {l}='{r}'");
        }
    } else if conf.hint {
        for (l, r) in hint(conf.lbuf, conf.rbuf) {
            println!("{l} --> {r}");
        }
    } else {
        if let Some(r) = expand(conf.lbuf, conf.rbuf) {
            println!("{r}");
        }
    }
    Ok(())
}
