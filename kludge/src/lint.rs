use std::{path::PathBuf, process::Command};

use anyhow::Context as _;

use crate::project::{self, Confidence};

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    pub(crate) files: Vec<PathBuf>,
}

fn run(cmd: &mut Command) -> anyhow::Result<()> {
    let status = cmd
        .status()
        .with_context(|| format!("Failed to run {}", cmd.get_program().to_string_lossy()))?;
    if !status.success() {
        anyhow::bail!("{} failed", cmd.get_program().to_string_lossy());
    }
    Ok(())
}

pub(super) fn go(conf: Config) -> anyhow::Result<()> {
    let mut project = project::project().cloned().unwrap_or_default();
    project.infer(Confidence::High);

    if let Some(cmd) = &project.lint {
        return run(&mut cmd.to_command(false));
    }

    // No project-level linter inferred; lint each file by extension
    for file in &conf.files {
        let ext = file.extension().and_then(|e| e.to_str());
        match ext {
            Some("py") => run(Command::new("ruff").args(["check", "--fix"]).arg(file))?,
            Some("sh") | Some("bash") | Some("zsh") => run(Command::new("shellcheck").arg(file))?,
            _ => {}
        }
    }
    Ok(())
}
