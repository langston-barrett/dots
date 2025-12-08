use std::{
    path::Path,
    process::{self, Command},
};

use anyhow::{Context as _, bail};

fn exec(mut cmd: Command) -> Result<(), anyhow::Error> {
    let status = cmd
        .status()
        .with_context(|| format!("Failed to run {:?} {:?}", cmd.get_program(), cmd.get_args()))?;
    if !status.success() {
        bail!("{:?} {:?} failed!", cmd.get_program(), cmd.get_args());
    }
    Ok(())
}

fn run(mut cmd: Command) -> Result<process::Output, anyhow::Error> {
    let out = cmd
        .output()
        .with_context(|| format!("Failed to run {:?} {:?}", cmd.get_program(), cmd.get_args()))?;
    if !out.status.success() {
        bail!(
            "{:?} {:?} failed!\n{}\n{}",
            cmd.get_program(),
            cmd.get_args(),
            String::from_utf8_lossy(out.stdout.as_slice()),
            String::from_utf8_lossy(out.stderr.as_slice())
        );
    }
    Ok(out)
}

fn changed_files_with_ext(ext: &'static str) -> Result<String, anyhow::Error> {
    let mut cmd = Command::new("git");
    cmd.args([
        "diff",
        "--diff-filter=d",
        "--name-only",
        "--cached",
        "--",
        ext,
    ]);
    let out = run(cmd)?;
    Ok(String::from_utf8_lossy(out.stdout.as_slice()).into_owned())
}

fn lint_py() -> Result<(), anyhow::Error> {
    let changed = changed_files_with_ext("*")?;
    let mut cmd = Command::new("scripts/lint/lint.py");
    cmd.arg("--format");
    exec(cmd)?;
    let mut cmd = Command::new("git");
    cmd.arg("add").args(changed.lines());
    run(cmd)?;

    exec(Command::new("scripts/lint/lint.py"))?;
    Ok(())
}

fn fourmolu() -> Result<(), anyhow::Error> {
    let hs = changed_files_with_ext("*.hs")?;
    let mut cmd = Command::new("fourmolu");
    cmd.args(["--mode", "inplace"]);
    cmd.args(hs.lines());
    exec(cmd)?;

    let mut cmd = Command::new("git");
    cmd.arg("add").args(hs.lines());
    run(cmd)?;
    Ok(())
}

fn hlint() -> Result<(), anyhow::Error> {
    let hs = changed_files_with_ext("*.hs")?;
    let mut cmd = Command::new("hlint");
    cmd.args(hs.lines());
    exec(cmd)?;
    Ok(())
}

fn clippy() -> Result<(), anyhow::Error> {
    let mut cmd = Command::new("cargo");
    cmd.args(["clippy", "--all-targets", "--", "--deny", "warnings"]);
    exec(cmd)?;
    Ok(())
}

pub(super) fn go() -> anyhow::Result<()> {
    if Path::new("scripts/lint/lint.py").exists() {
        lint_py()?;
        return Ok(());
    }

    if Path::new("fourmolu.yml").exists() || Path::new("fourmolu.yaml").exists() {
        fourmolu()?;
    }

    if Path::new(".hlint.yml").exists() || Path::new(".hlint.yaml").exists() {
        hlint()?;
    }

    if Path::new("Cargo.toml").exists() {
        // TODO: `cargo fmt`, but just for `langston-barrett` repos
        clippy()?;
    }

    Ok(())
}
