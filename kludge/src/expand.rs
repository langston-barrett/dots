// def: kludge-expand

use std::{env, path::Path};

use crate::project::{self, PROJECTS};
use crate::system as build;

const CURSOR: char = '•';

/// Expand shell abbreviations
#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[clap(long)]
    aliases: bool,
    #[clap(long)]
    hint: bool,
    #[clap(long)]
    enter: bool,
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
        if Path::new("scripts/lint/lint.py").exists() {
            return Some(String::from("scripts/lint/lint.py --format "));
        }
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
        if Path::new("scripts/lint/lint.py").exists() {
            return Some(String::from("scripts/lint/lint.py "));
        }
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

// Expansions that only apply when the user hit "enter", not "space"
const ANYWHERE_ENTER: &[(&str, &str)] = &[
    ("gclg", "git clone https://github.com/GaloisInc/_"),
    ("gclh", "git clone https://github.com/_"),
    ("gclm", "git clone https://github.com/langston-barrett/_"),
    ("mkcd", "mkdir _ && cd _"),
];

const ANYWHERE: &[(&str, &str, &str)] = &[
    ("ba", "cabal build all", ""),
    ("bc", CLANG_LLVM, ""),
    ("cb", "cabal", ""),
    ("chx", "chmod +x", ""),
    ("cg", "cargo", ""),
    ("cgi.", "cargo install --path=.", ""),
    ("cpwd", "pwd | copy", ""),
    ("curls", CURLS, ""),
    ("dk", "docker", ""),
    ("dk-clang", DOCKER_CLANG, "'"),
    ("dk-dev", DOCKER_DEV, "'"),
    ("e", "kludge edit", ""),
    ("hex", "python3 -c 'print(hex(", "))'"),
    ("last", r#"printf '%s\n' "$history[$((HISTCMD-1))]""#, ""),
    ("ll", CLANG_LLVM_S, ""),
    ("lower", "tr '[:upper:]' '[:lower:]'", ""),
    ("m", "make", ""),
    ("od", "objdump", ""),
    ("pr", "gh pr create --assignee langston-barrett --web", ""),
    ("py3", "python3", ""),
    ("pye", "python3 -c 'print(", ")'"),
    ("qr", "qrencode -t utf8", ""),
    (
        "recent-branches",
        "git branch --sort=-committerdate | head -n 10",
        "",
    ),
    ("rgall", "rg --hidden --no-ignore", ""),
    ("rmrf", "\\rm -rf", ""),
    ("sky", "ssh sky", ""),
    ("todo", "hx ~/todo.md", ""),
    ("top", "cd $(git rev-parse --show-toplevel)", ""),
    ("tp", "trash put", ""),
    ("upper", "tr '[:lower:]' '[:upper:]'", ""),
    ("y", "copy", ""),
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
    ("gco-", "git checkout -", ""),
    ("gcom", GIT_CHECKOUT_MAIN, ""),
    (
        "gcor",
        "git checkout $(git branch --sort=-committerdate --format='%(refname:short)' | head -n 8 | pick)",
        "",
    ),
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
    ("kl", "kludge launcher", ""),
];

fn notify(s: String) {
    drop(std::process::Command::new("notify").arg(s).spawn());
}

// duplicated from zbr
fn clean_buf(mut lbuf: String) -> (String, String) {
    let mut prefix = String::new();
    for delim in [" || ", " && ", "; ", "| ", "|& "] {
        if let Some(idx) = lbuf.rfind(delim) {
            let after = idx + delim.len();
            let (pre, post) = lbuf.split_at(after);
            prefix = String::from(pre);
            lbuf = String::from(post);
        }
    }
    (prefix, lbuf)
}

fn expand_anywhere(lbuf0: &str, rbuf0: &str, enter: bool) -> Option<(String, String)> {
    let (prefix, lbuf0) = clean_buf(String::from(lbuf0));
    for (short, lbuf, rbuf) in ANYWHERE.iter().copied() {
        if lbuf0 == short && rbuf0.is_empty() {
            return Some((format!("{prefix}{lbuf}"), rbuf.to_owned()));
        }
        if lbuf0 == lbuf && !lbuf.contains(' ') {
            notify(format!("hint: try {short}"));
        }
    }
    if enter {
        for (short, long) in ANYWHERE_ENTER {
            if let Some(rest) = lbuf0.strip_prefix(&format!("{short} ")) {
                return Some((
                    format!("{prefix}{}", long.replace('_', rest)),
                    String::new(),
                ));
            }
        }
    }
    None
}

fn build_command(cmd: &str, args: &[&str]) -> String {
    if args.is_empty() {
        cmd.to_string()
    } else {
        format!("{} {}", cmd, args.join(" "))
    }
}

fn project_expansions(project: &project::Project) -> Vec<(&str, String)> {
    let mut expansions: Vec<(&str, String)> = Vec::new();

    if let Some((cmd, args)) = project.lint {
        expansions.push(("l", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = project.format {
        expansions.push(("f", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = project.build {
        expansions.push(("b", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = project.test {
        expansions.push(("t", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = project.run {
        expansions.push(("r", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = project.watch {
        expansions.push(("w", build_command(cmd, args)));
    }
    for (shortcut, command) in project.aliases.iter().copied() {
        expansions.push((shortcut, command.to_string()));
    }
    expansions
}

fn expand_project(lbuf: &str, rbuf: &str) -> Option<(String, String)> {
    let name = project::git_root_name()?;
    let project = PROJECTS.iter().find(|p| p.name == name)?;
    for (l, r) in project_expansions(project) {
        // TODO: Allow non-empty rbufs
        if lbuf == l && rbuf.is_empty() {
            return Some((r, String::new()));
        }
    }
    None
}

// Perform arbitrary transformations
fn expand_advanced(lbuf: &str, rbuf: &str, enter: bool) -> Option<(String, String)> {
    if !enter {
        return None;
    }

    // turn `each` into `xargs`
    let mut changed = false;
    let mut words = Vec::with_capacity(8);
    for word in lbuf.split_whitespace() {
        if word == "each" {
            words.push("xargs");
            words.push("-I");
            words.push("{}");
            changed = true;
        } else {
            words.push(word);
        }
    }
    if changed {
        return Some((words.join(" "), rbuf.to_owned()));
    }

    None
}

fn expand(lbuf: String, rbuf: String, enter: bool) -> Option<(String, String)> {
    expand_project(&lbuf, &rbuf)
        .or_else(|| expand_anywhere(&lbuf, &rbuf, enter))
        .or_else(|| expand_build_system(&lbuf).map(|s| (s, String::new())))
        .or_else(|| expand_advanced(&lbuf, &rbuf, enter))
}

fn to_hint(expanded: &str) -> String {
    expanded.replace('\n', " ").chars().take(60).collect()
}

// TODO: Deduplicate logic
fn hint(lbuf0: String, rbuf0: String) -> Vec<(&'static str, String)> {
    let mut results = Vec::with_capacity(8);
    for (short, lbuf, rbuf) in ANYWHERE.iter().copied() {
        if short.starts_with(lbuf0.as_str()) && rbuf0.is_empty() {
            let hint = to_hint(&format!("{lbuf}{CURSOR}{rbuf}"));
            results.push((short, hint));
        }
    }
    for (short, long) in ANYWHERE_ENTER {
        if let Some(rest) = lbuf0.strip_prefix(&format!("{short} ")) {
            let hint = to_hint(&long.replace('_', rest));
            results.push((short, hint));
        }
    }
    if let Some(name) = project::git_root_name()
        && let Some(project) = PROJECTS.iter().find(|p| p.name == name)
    {
        let expansions = project_expansions(project);
        for (l, r) in expansions {
            if l.starts_with(lbuf0.as_str()) && rbuf0.is_empty() {
                let hint = to_hint(&r);
                results.push((l, hint));
            }
        }
    }
    results
}

#[allow(clippy::unnecessary_wraps)]
pub(super) fn go(conf: Config) -> anyhow::Result<()> {
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
    } else if let Some((lbuf, rbuf)) = expand(conf.lbuf, conf.rbuf, conf.enter) {
        println!("{lbuf}{CURSOR}{rbuf}");
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::expand;

    fn test_expand(l: &str, r: &str) -> Option<(String, String)> {
        expand(l.to_owned(), r.to_owned(), false)
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
