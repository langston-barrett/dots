use std::process::Command;

use anyhow::{Context as _, bail};

use crate::project;

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

fn remove_claude_metadata() -> Result<(), anyhow::Error> {
    let mut cmd = Command::new("git");
    cmd.args(["log", "-1", "--pretty=%B"]);
    let out = cmd.output().context("Failed to get commit message")?;

    if !out.status.success() {
        bail!("Failed to read commit message");
    }

    let message = String::from_utf8_lossy(&out.stdout);

    let cleaned: Vec<&str> = message
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !(trimmed.starts_with("Co-Authored-By:") && trimmed.contains("noreply@anthropic.com"))
        })
        .collect();

    if cleaned.len() != message.lines().count() {
        let cleaned_message = cleaned.join("\n");
        let mut cmd = Command::new("git");
        cmd.args(["commit", "--amend", "--reset-author", "-m"])
            .arg(cleaned_message);

        exec(cmd)?;
    }

    Ok(())
}

fn run_checks() -> Result<(), anyhow::Error> {
    let mut project = project::project().cloned().unwrap_or_default();
    project.infer(project::Confidence::High);
    if let Some(cmd) = &project.format {
        exec(cmd.to_command(true))?;
    }
    if let Some(cmd) = &project.lint {
        exec(cmd.to_command(true))?;
    }
    Ok(())
}

pub(super) fn go() -> anyhow::Result<()> {
    remove_claude_metadata()?;
    run_checks()?;
    Ok(())
}
