use std::{path::PathBuf, process::Command};

use anyhow::Context as _;

use crate::project::{self, Confidence};

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[arg(default_value = "")]
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

    if let Some(cmd) = &project.format {
        return run(&mut cmd.to_command(false));
    }

    if conf.files.is_empty() {
        return Ok(());
    }

    // No project-level formatter inferred; format each file by extension
    for file in &conf.files {
        let ext = file.extension().and_then(|e| e.to_str());
        match ext {
            Some("rs") => run(Command::new("rustfmt").arg(file))?,
            Some("hs") => run(Command::new("fourmolu")
                .args(["--mode", "inplace"])
                .arg(file))?,
            Some("py") => run(Command::new("ruff").args(["format"]).arg(file))?,
            _ => {}
        }
    }
    Ok(())
}
