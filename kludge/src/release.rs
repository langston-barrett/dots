use std::process::Command;

use anyhow::Context as _;

pub(super) fn go() -> anyhow::Result<()> {
    Command::new("git")
        .arg("checkout")
        .arg("main")
        .output()
        .context("failed to execute `git checkout main`")?;
    Ok(())
}
