use std::{env, error::Error, ffi::OsStr};

use crate::system as build;

const CURSOR: char = '•';

/// Expand shell abbreviations
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
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal build ")),
            Some(build::System::Cargo) => Some(String::from("cargo build ")),
            Some(build::System::Make) => Some(String::from("make ")),
            None => None,
        }
    } else if lbuf == "d" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal haddock ")),
            _ => None,
        }
    } else if lbuf == "f" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from(
                "fourmolu --mode inplace $(git ls-files '*.hs') ",
            )),
            Some(build::System::Cargo) => Some(String::from("cargo fmt ")),
            Some(build::System::Make) => Some(String::from("make fmt ")),
            _ => None,
        }
    } else if lbuf == "i" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal install ")),
            Some(build::System::Cargo) => Some(String::from("cargo install ")),
            Some(build::System::Make) => Some(String::from("make install ")),
            _ => None,
        }
    } else if lbuf == "l" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cargo) => Some(String::from(
                "cargo clippy --all-targets -- --deny warnings ",
            )),
            Some(build::System::Make) => Some(String::from("make lint ")),
            _ => None,
        }
    } else if lbuf == "r" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal run ")),
            Some(build::System::Cargo) => Some(String::from("cargo run ")),
            Some(build::System::Make) | None => None,
        }
    } else if lbuf == "t" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("cabal test ")),
            Some(build::System::Cargo) => Some(String::from("cargo test ")),
            Some(build::System::Make) => Some(String::from("make test ")),
            None => None,
        }
    } else if lbuf == "w" {
        let pwd = env::current_dir().ok()?;
        match build::System::detect(pwd) {
            Some(build::System::Cabal) => Some(String::from("ghcid")),
            Some(build::System::Cargo) => Some(String::from(
                "ls ./**/Cargo.toml ./**/*.rs | entr -c -s 'cargo fmt && cargo clippy --all-targets -- --deny warnings'",
            )),
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
const DOCKER_CLANG: &str = r#"
docker run \
  --platform linux/amd64 \
  --rm \
  --mount "type=bind,src=${PWD},dst=/work" \
  --workdir /work \
  ubuntu:24.04 \
  sh -c 'apt-get update && apt-get install -y clang && clang"#;
const DOCKER_DEV: &str = r#"
docker run \
  --platform linux/amd64 \
  --rm \
  --interactive \
  --tty \
  --mount "type=bind,src=${PWD},dst=/work" \
  --workdir /work \
  --env "PROMPT_EXTRA=${1} : " \
  --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
  --mount type=bind,readonly=true,src=$HOME/.config/bash,dst=/root/.config/bash \
  --mount type=bind,readonly=true,src=$HOME/code/dots/files/bashrc,dst=/root/.bashrc \
  --mount type=bind,readonly=true,src=$HOME/.config/sh.d,dst=/root/.config/sh.d \
  ubuntu-dev
"#;
const GIT_CHECKOUT_MAIN: &str = "git checkout $(git branch | grep -Eo '(main|master)$')";
const GIT_DIFF_MAIN: &str = "git diff $(git branch | grep -Eo '(main|master)$')";
const GIT_MERGE_ORIGIN_MAIN: &str = "git merge origin/$(git branch | grep -Eo '(main|master)$')";
const GIT_MERGE_UPSTREAM_MAIN: &str =
    "git merge upstream/$(git branch | grep -Eo '(main|master)$')";
const GIT_PULL_ORIGIN_MAIN: &str = "git pull origin $(git branch | grep -Eo '(main|master)$')";
const GIT_PULL_UPSTREAM_MAIN: &str = "git pull upstream $(git branch | grep -Eo '(main|master)$')";
const GIT_REBASE_MAIN: &str = "git rebase $(git branch | grep -Eo '(main|master)$')";
const GIT_REBASE_INTERACTIVE_MAIN: &str =
    "git rebase --interactive $(git branch | grep -Eo '(main|master)$')";
const GIT_REBASE_ORIGIN_MAIN: &str = "git rebase origin/$(git branch | grep -Eo '(main|master)$')";
const GIT_REBASE_INTERACTIVE_ORIGIN_MAIN: &str =
    "git rebase --interactive origin/$(git branch | grep -Eo '(main|master)$')";
const GIT_RESET_HARD_ORIGIN_MAIN: &str =
    "git reset --hard origin/$(git branch | grep -Eo '(main|master)$')";

const ANYWHERE: &[(&str, &str, &str)] = &[
    ("ba", "cabal build all", ""),
    ("bc", CLANG_LLVM, ""),
    ("cb", "cabal", ""),
    ("cg", "cargo", ""),
    ("cgi.", "cargo install --path=.", ""),
    ("cpwd", "pwd | copy", ""),
    ("curls", CURLS, ""),
    ("dk", "docker", ""),
    ("dk-clang", DOCKER_CLANG, "'"),
    ("dk-dev", DOCKER_DEV, "'"),
    ("e", "hx", ""),
    ("hex", "python3 -c 'print(hex(", "))'"),
    ("ll", CLANG_LLVM_S, ""),
    ("m", "make", ""),
    ("od", "objdump", ""),
    ("pr", "gh pr create --assignee langston-barrett --web", ""),
    ("py3", "python3", ""),
    ("pye", "python3 -c 'print(", ")'"),
    ("rgall", "rg --hidden --no-ignore", ""),
    ("rmrf", "\\rm -rf", ""),
    ("sky", "ssh sky", ""),
    ("todo", "hx ~/todo.md", ""),
    ("tp", "trash put", ""),
    ("y", "clipboard", ""),
    //
    // nix
    //
    ("nb", "nix-build", ""),
    ("nc", "nix-channel", ""),
    ("nba", "nix-build -A", ""),
    ("ns", "nix-shell", ""),
    ("nsr", "nix-shell --run", ""),
    ("nsrzsh", "nix-shell --run 'exec zsh'", ""),
    //
    // git
    //
    // see also .gitconfig, zbr
    //
    ("ga.", "git add .", ""),
    ("gc.", "git commit --message .", ""),
    ("gca", "git commit --amend", ""),
    ("gcb", "git checkout -b", ""),
    ("gclg", "git clone https://github.com/GaloisInc/", ""),
    ("gclh", "git clone https://github.com/", ""),
    ("gclm", "git clone https://github.com/langston-barrett/", ""),
    ("gco-", "git checkout -", ""),
    ("gcom", GIT_CHECKOUT_MAIN, ""),
    ("gdm", GIT_DIFF_MAIN, ""),
    ("gds", "git diff --cached", ""),
    ("gfo", "git fetch origin", ""),
    ("gfu", "git fetch upstream", ""),
    ("gmom", GIT_MERGE_ORIGIN_MAIN, ""),
    ("gmum", GIT_MERGE_UPSTREAM_MAIN, ""),
    ("gplm", "git pull mine", ""),
    ("gplo", "git pull origin", ""),
    ("gplom", GIT_PULL_ORIGIN_MAIN, ""),
    ("gplu", "git pull upstream", ""),
    ("gplum", GIT_PULL_UPSTREAM_MAIN, ""),
    ("grbim", GIT_REBASE_INTERACTIVE_MAIN, ""),
    ("grbiom", GIT_REBASE_INTERACTIVE_ORIGIN_MAIN, ""),
    ("grbm", GIT_REBASE_MAIN, ""),
    ("grbom", GIT_REBASE_ORIGIN_MAIN, ""),
    ("grhom", GIT_RESET_HARD_ORIGIN_MAIN, ""),
    ("grph", "git rev-parse HEAD", ""),
    ("grv", "git remote --verbose", ""),
    ("gsuud", "git submodule update", ""), // TODO: zbr should handle this
    ("gsuudi", "git submodule update --init", ""), // TODO: zbr should handle this
    //
    // macos
    //
    #[cfg(target_os = "macos")]
    ("trailing", "sed -i '' 's/[[:space:]]*$//'", ""),
    //
    // linux
    //
    #[cfg(target_os = "linux")]
    ("docker", "sudo -g docker docker", ""),
    #[cfg(target_os = "linux")]
    ("trailing", "sed -i 's/[ \t]*$//", ""),
    #[cfg(target_os = "linux")]
    ("sys", "sudo systemctl", ""),
    #[cfg(target_os = "linux")]
    ("syss", "sudo systemctl status", ""),
    #[cfg(target_os = "linux")]
    ("sysr", "sudo systemctl restart", ""),
    #[cfg(target_os = "linux")]
    ("sysu", "systemctl --user", ""),
    #[cfg(target_os = "linux")]
    ("sysus", "systemctl --user status", ""),
    #[cfg(target_os = "linux")]
    ("sysur", "systemctl --user restart", ""),
    //
    // meta
    //
    ("k", "kludge", ""),
    ("ka", "hx ~/code/dots/kludge/src/expand.rs", ""),
    (
        "ki",
        "cd ~/code/dots/kludge; cargo install --path=.; cd -",
        "",
    ),
];

fn expand_anywhere(lbuf0: &str, rbuf0: &str) -> Option<(String, String)> {
    for (short, lbuf, rbuf) in ANYWHERE.iter().copied() {
        if lbuf0 == short && rbuf0.is_empty() {
            return Some((lbuf.to_owned(), rbuf.to_owned()));
        }
    }
    None
}

const GREASE_CMDS: &[(&str, &str)] = &[
    ("r", "cabal run exe:grease --"),
    ("t", "cabal run test:grease-tests --"),
    // TODO
    (
        "l",
        "hlint grease{,-aarch32,-ppc,-x86}/src grease-cli/src grease-exe/{main,src,tests}",
    ),
    (
        "to",
        "cabal run exe:grease -- --symbol test $(fd --type=x elf tests/ | zshfzf)",
    ),
    (
        "w",
        "ghcid --command \"cabal repl lib:grease pkg:grease-cli pkg:grease-exe\"",
    ),
    ("wt", "ghcid --target=test:grease-tests"),
];

const SCREACH_CMDS: &[(&str, &str)] = &[
    ("r", "cabal run exe:screach --"),
    (
        "l",
        "hlint --hint=../deps/grease/.hlint.yaml {app,src,test} ../elf-edit-ecfs/{src,tools}",
    ),
    ("t", "cabal run test:screach-test --"),
    (
        "w",
        "ghcid --command \"cabal repl lib:screach exe:screach\"",
    ),
    ("wt", "ghcid --target=test:screach-test"),
];

const BASIC: &[(&str, &[(&str, &str)])] = &[
    (
        "crucible-llvm-cli",
        &[
            ("r", "cabal run exe:crucible-llvm --"),
            ("rs", "cabal run exe:crucible-llvm -- simulate"),
            ("t", "cabal run test:crucible-llvm-cli-tests --"),
            ("w", "ghcid"),
            ("wt", "ghcid --target=test:crucible-llvm-cli-tests"),
        ],
    ),
    (
        "detect",
        &[
            (
                "bs",
                "echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid && sudo sysctl kernel.perf_event_mlock_kb=2048 && cargo b -q --profile=profiling --bin=sofuzz && samply record ./target/profiling/sofuzz --solutions /run/user/1000/sols --gas=2048 sofuzz/rs/map/map.toml target/profiling/libsofuzz_map.so --no-check-dwarf",
            ),
            (
                "e1",
                "rm -rf benign solutions ; cargo build -p=eval1-smi-model && cargo run --bin dxezz -- --qcow=targets/eval1-smi/image-debug/snapshots.qcow2 targets/eval1-smi/eval1-smi-debug.toml target/debug/libeval1_smi_model.so --seed=1 --outer-iterations=8 --inner-iterations=1 --no-check-snapshots -v",
            ),
            (
                "lu",
                "cargo clippy --all-targets --no-default-features --features=usermode -- --deny warnings",
            ),
            ("rb", "cargo run --bin=bzro --"),
            (
                "rbu",
                "cargo run --bin=bzro --no-default-features --features=usermode --",
            ),
            ("rd", "cargo run --bin=dxezz --"),
            ("rs", "cargo run --bin=sofuzz --"),
            ("t", "cargo test"),
            ("tu", "cargo test --no-default-features --features=usermode"),
            ("tb", "cargo test --package=bzro -- --test-threads=1"),
            (
                "tbu",
                "cargo test --package=bzro -- --test-threads=1  --no-default-features --features=usermode",
            ),
            ("td", "cargo test --package=dxezz -- --test-threads=1"),
            (
                "ts",
                "cargo b -q --package=sofuzz-boxcar && cargo b -q --package=sofuzz-map && cargo test --package=sofuzz",
            ),
        ],
    ),
    (
        "kludge",
        &[(
            "l",
            "cargo fmt --check && cargo clippy --all-targets -- --deny warnings",
        )],
    ),
    ("grease", GREASE_CMDS),
    ("grease-cli", GREASE_CMDS),
    ("grease-exe", GREASE_CMDS),
    ("screach", SCREACH_CMDS),
];

fn expand_basic(lbuf: &str, rbuf: &str) -> Option<(String, String)> {
    let cwd = env::current_dir().ok()?;
    for (d, expands) in BASIC.iter().copied() {
        let name = cwd.as_path().file_name().and_then(OsStr::to_str);
        if name == Some(d) {
            for (l, r) in expands.iter().copied() {
                // TODO: Allow non-empty rbufs
                if lbuf == l && rbuf.is_empty() {
                    return Some((r.to_owned(), String::new()));
                }
            }
        }
    }
    None
}

fn expand(lbuf: String, rbuf: String) -> Option<(String, String)> {
    expand_basic(&lbuf, &rbuf)
        .or_else(|| expand_anywhere(&lbuf, &rbuf))
        .or_else(|| expand_build_system(&lbuf).map(|s| (s, String::new())))
}

// TODO: Deduplicate logic
fn hint(lbuf0: String, rbuf0: String) -> Vec<(&'static str, String)> {
    let mut results = Vec::with_capacity(8);
    for (short, lbuf, rbuf) in ANYWHERE.iter().copied() {
        if short.starts_with(lbuf0.as_str()) && rbuf0.is_empty() {
            results.push((short, format!("{lbuf}{CURSOR}{rbuf}")));
        }
    }
    if let Ok(cwd) = env::current_dir() {
        let name = cwd.as_path().file_name().and_then(OsStr::to_str);
        for (d, expands) in BASIC.iter().copied() {
            if name == Some(d) {
                for (l, r) in expands.iter().copied() {
                    if l.starts_with(lbuf0.as_str()) && rbuf0.is_empty() {
                        results.push((l, r.to_owned()));
                    }
                }
            }
        }
    }
    results
}

#[allow(clippy::unnecessary_wraps)]
pub(super) fn go(conf: Config) -> Result<(), Box<dyn Error>> {
    // TODO: Help system
    if conf.aliases {
        for (short, lbuf, rbuf) in ANYWHERE {
            if rbuf.is_empty() && !lbuf.contains('\'') {
                println!("alias {short}='{lbuf}'");
            }
        }
    } else if conf.hint {
        for (l, r) in hint(conf.lbuf, conf.rbuf).iter().take(5) {
            println!("{l} --> {r}");
        }
    } else if let Some((lbuf, rbuf)) = expand(conf.lbuf, conf.rbuf) {
        println!("{lbuf}{CURSOR}{rbuf}");
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::expand;

    fn test_expand(l: &str, r: &str) -> Option<(String, String)> {
        expand(l.to_owned(), r.to_owned())
    }

    fn test_expand_is(l: &str, r: &str, result: &str) {
        assert_eq!(test_expand(l, r), Some((result.to_owned(), String::new())));
    }

    #[test]
    fn expand_l() {
        test_expand_is(
            "l",
            "",
            "cargo fmt --check && cargo clippy --all-targets -- --deny warnings",
        );
    }
}
