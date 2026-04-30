use std::{
    env,
    path::Path,
    process::{self, Command},
};

use anyhow::{Context as _, bail};

use crate::{project, typos_advisory};

fn exec(mut cmd: Command) -> Result<(), anyhow::Error> {
    eprintln!("{}", project::print_cmd(&cmd));
    let status = cmd
        .status()
        .with_context(|| format!("Failed to run {:?} {:?}", cmd.get_program(), cmd.get_args()))?;
    if !status.success() {
        bail!("{:?} {:?} failed!", cmd.get_program(), cmd.get_args());
    }
    Ok(())
}

fn run(mut cmd: Command) -> Result<process::Output, anyhow::Error> {
    eprintln!("{}", project::print_cmd(&cmd));
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

fn cargo_fmt() -> Result<(), anyhow::Error> {
    let mut cmd = Command::new("cargo");
    cmd.arg("fmt");
    exec(cmd)?;
    Ok(())
}

fn clippy() -> Result<(), anyhow::Error> {
    let mut cmd = Command::new("cargo");
    cmd.args(["clippy", "--all-targets", "--", "--deny", "warnings"]);
    exec(cmd)?;
    Ok(())
}

fn mine(linted: bool) -> Result<(), anyhow::Error> {
    if !linted && Path::new("Cargo.toml").exists() {
        clippy()?;
        cargo_fmt()?;
    }
    if !Path::new("LICENSE").exists() {
        bail!("No LICENSE?");
    }
    Ok(())
}

fn check_claude_metadata() -> Result<(), anyhow::Error> {
    // Check .git/config for Claude-related metadata
    let git_config_path = Path::new(".git/config");
    if git_config_path.exists() {
        let config_content =
            std::fs::read_to_string(git_config_path).context("Failed to read .git/config")?;

        if config_content.contains("claude") || config_content.contains("Claude") {
            bail!("Claude metadata found in .git/config - please remove before committing");
        }
    }

    Ok(())
}

pub(super) fn go() -> anyhow::Result<()> {
    if env::var("KLUDGE_SKIP_PRE_COMMIT").is_ok_and(|v| v != "0") {
        return Ok(());
    }

    let mut linted = false;
    let mut project = project::project().cloned().unwrap_or_default();
    project.infer(project::Confidence::High);
    if let Some(cmd) = &project.format {
        let changed = changed_files_with_ext("*")?;
        exec(cmd.to_command(true))?;
        let mut cmd = Command::new("git");
        cmd.arg("add").args(changed.lines());
        run(cmd)?;
    }
    if let Some(cmd) = &project.lint {
        linted = true;
        exec(cmd.to_command(true))?;
    }

    if project::mine() {
        mine(linted)?;
        check_claude_metadata()?;
    }

    typos_advisory::run(true);

    Ok(())
}
