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
            Some(build::System::Cabal) => Some(String::from("cabal build ")),
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

const CLANG_LLVM: &str = "clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0";
const CLANG_LLVM_S: &str = "clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0 -S";
const CURLS: &str = "curl \\
  --fail \\
  --location \\
  --proto '=https' \\
  --show-error \\
  --silent \\
  --tlsv1.2 \\";

const ANYWHERE: &[(&str, &str)] = &[
    ("bc", CLANG_LLVM),
    ("cb", "cabal"),
    ("cg", "cargo"),
    ("curls", CURLS),
    ("dk", "docker"),
    ("e", "hx"),
    ("ll", CLANG_LLVM_S),
    ("m", "make"),
    ("od", "objdump"),
    ("py3", "python3"),
    ("rgall", "rg --hidden --no-ignore"),
    ("rmrf", "\\rm -rf"),
    ("sky", "ssh sky"),
    ("todo", "hx ~/todo.md"),
    ("tp", "trash put"),
    ("y", "clipboard"),
    //
    // nix
    //
    ("nb", "nix-build"),
    ("nc", "nix-channel"),
    ("nba", "nix-build -A"),
    ("ns", "nix-shell"),
    ("nsr", "nix-shell --run"),
    ("nsrzsh", "nix-shell --run 'exec zsh'"),
    //
    // git
    //
    // see also .gitconfig
    //
    ("gclh", "git clone https://github.com/"),
    ("gclg", "git clone https://github.com/GaloisInc/"),
    ("gclm", "git clone https://github.com/langston-barrett/"),
    //
    ("ga", "git add"),
    ("gau", "git add --update"),
    ("gb", "git branch"),
    ("gbD", "git branch -D"),
    ("gbl", "git blame"),
    ("gbr", "git branch"),
    ("gca", "git commit --amend"),
    ("gcb", "git checkout -b"),
    ("gcl", "git clone --jobs 4"),
    ("gcm", "git commit -m"),
    ("gcm", "git commit"),
    ("gcmm", "git commit --message ."),
    ("gco", "git checkout"),
    ("gcom", "git checkout main"),
    ("gcp", "git cherry-pick"),
    ("gd", "git diff"),
    ("gdm", "git diff master"),
    ("gds", "git diff --cached"),
    ("gf", "git fetch"),
    ("gfa", "git fetch --all"),
    ("gFp", "git pull origin"),
    ("gFu", "git pull upstream"),
    ("ghd", "git rev-parse HEAD"),
    ("gi", "git init"),
    ("gl", "git log"),
    ("glsf", "git ls-files"),
    ("gm", "git merge"),
    ("gmum", "git merge upstream/master"),
    ("gp", "git push"),
    ("gpf", "git push --force-with-lease"),
    ("gPf", "git push --force-with-lease"),
    ("gpl", "git pull"),
    ("gplm", "git pull mine"),
    ("gplo", "git pull origin"),
    ("gplu", "git pull upstream"),
    ("gPp", "git push -u origin"),
    ("gpum", "git pull upstream master"),
    ("gr", "git reset"),
    ("gra", "git rebase --abort"),
    ("grb", "git rebase"),
    ("grc", "git rebase --continue"),
    ("grhm", "git reset --hard origin/master"),
    ("gri", "git rebase --interactive"),
    ("grv", "git remote --verbose"),
    ("gs", "git status"),
    ("gsh", "git stash"),
    ("gss", "git status --short"),
    ("gsu", "git submodule"),
    ("gsup", "git submodule update"),
    ("gsupi", "git submodule update --init"),
    ("gt", "git tag"),
    ("gwa", "git worktree add"),
    ("gwl", "git worktree list"),
    ("gwm", "git worktree move"),
    ("gwr", "git worktree remove"),
    //
    // macos
    //
    #[cfg(target_os = "macos")]
    ("trailing", "sed -i '' 's/[[:space:]]*$//'"),
    //
    // linux
    //
    #[cfg(target_os = "linux")]
    ("docker", "sudo -g docker docker"),
    #[cfg(target_os = "linux")]
    ("trailing", "sed -i 's/[ \t]*$//"),
    #[cfg(target_os = "linux")]
    ("sys", "sudo systemctl"),
    #[cfg(target_os = "linux")]
    ("syss", "sudo systemctl status"),
    #[cfg(target_os = "linux")]
    ("sysr", "sudo systemctl restart"),
    #[cfg(target_os = "linux")]
    ("sysu", "systemctl --user"),
    #[cfg(target_os = "linux")]
    ("sysus", "systemctl --user status"),
    #[cfg(target_os = "linux")]
    ("sysur", "systemctl --user restart"),
    //
    // meta
    //
    ("k", "kludge"),
    ("ka", "hx ~/code/dots/kludge/src/expand.rs"),
    ("ki", "cd ~/code/dots/kludge; cargo install --path=.; cd -"),
];

fn expand_anywhere(lbuf: &str, rbuf: &str) -> Option<String> {
    for (l, r) in ANYWHERE {
        if lbuf == *l && rbuf.is_empty() {
            return Some(r.to_string());
        }
    }
    None
}

const BASIC: &[(&str, &[(&str, &str)])] = &[
    ("crucible-llvm-cli", &[
        ("r", "cabal run exe:crucible-llvm --"),
        ("rs", "cabal run exe:crucible-llvm -- simulate"),
        ("t", "cabal run test:crucible-llvm-cli-tests --"),
        ("w", "ghcid"),
        ("wt", "ghcid --target=test:crucible-llvm-cli-tests"),
    ]),
    ("detect", &[
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
    ]),
    ("grease", &[
        ("r", "cabal run exe:grease --"),
        ("t", "cabal run test:grease-tests --"),
        // TODO
        ("to", "cabal run exe:grease -- --symbol test $(fd --type=x elf tests/ | zshfzf)"),
        ("w", "ghcid"),
        ("wt", "ghcid --target=test:grease-tests"),
    ])
];

fn expand_basic(lbuf: &str, rbuf: &str) -> Option<String> {
    let cwd = env::current_dir().ok()?;
    for (d, expands) in BASIC {
        let name = cwd.as_path().file_name().and_then(OsStr::to_str);
        if name == Some(d) {
            for (l, r) in *expands {
                if lbuf == *l && rbuf.is_empty() {
                    return Some(r.to_string());
                }
            }
        }
    }
    None
}

fn expand(lbuf: String, rbuf: String) -> Option<String> {
    expand_basic(&lbuf, &rbuf)
        .or_else(|| expand_anywhere(&lbuf, &rbuf))
        .or_else(|| expand_build_system(&lbuf))
}

// TODO: Deduplicate logic
fn hint(lbuf: String, rbuf: String) -> Vec<(&'static str, &'static str)> {
    let mut results = Vec::with_capacity(8);
    for (l, r) in ANYWHERE {
        if l.starts_with(lbuf.as_str()) && rbuf.is_empty() {
            results.push((*l, *r));
        }
    }
    if let Ok(cwd) = env::current_dir() {
        let name = cwd.as_path().file_name().and_then(OsStr::to_str);
        for (d, expands) in BASIC {
            if name == Some(d) {
                for (l, r) in *expands {
                    if l.starts_with(lbuf.as_str()) && rbuf.is_empty() {
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
        for (l, r) in hint(conf.lbuf, conf.rbuf).iter().take(5) {
            println!("{l} --> {r}");
        }
    } else if let Some(r) = expand(conf.lbuf, conf.rbuf) {
        println!("{r}");
    }
    Ok(())
}
