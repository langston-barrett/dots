use std::{env, process::Command};

use anyhow::Context as _;

// NB: Spacing is a Unicode em-space
const PROMPT_SEP: &str = " : ";

fn git_branch_name() -> Option<String> {
    let out = Command::new("git")
        .arg("symbolic-ref")
        .arg("--short")
        .arg("HEAD")
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let nm = String::from_utf8_lossy(out.stdout.as_slice());
    Some(nm.trim_end().to_string())
}

pub(super) fn go() -> anyhow::Result<()> {
    let current_dir = env::current_dir().context("failed to get current directory")?;
    for dir in current_dir.ancestors() {
        if dir.join(".git").is_dir() {
            if let Some(nm) = git_branch_name() {
                println!("{PROMPT_SEP}{nm}");
            }
            break;
        }
    }
    Ok(())
}
