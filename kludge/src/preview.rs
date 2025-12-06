use std::{fs, io::Read as _, os::unix::process::CommandExt as _, path::PathBuf, process};

use anyhow::Context as _;

/// Preview a file (e.g., for fzf)
#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    input: String,
}

// https://github.com/Aloxaf/fzf-tab/wiki/Preview#show-file-contents
pub(super) fn go(conf: Config) -> anyhow::Result<()> {
    let path = PathBuf::from(&conf.input);
    if path.is_dir() {
        Err(process::Command::new("ls")
            .arg("--color=always")
            .arg(&path)
            .exec())
        .with_context(|| format!("failed to execute ls for directory {}", path.display()))?;
    }

    if let Ok(true) = path.try_exists() {
        let bs = {
            let mut f = fs::File::open(&path)
                .with_context(|| format!("failed to open file {}", path.display()))?;
            let mut buf = vec![0u8; 16];
            let read = f
                .read(&mut buf)
                .with_context(|| format!("failed to read from file {}", path.display()))?;
            buf.truncate(read);
            buf
        };
        let is_ascii = bs.iter().all(|&b| b <= 0x7F);
        if is_ascii {
            Err(process::Command::new("bat")
                .arg("--color=always")
                .arg("--plain")
                .arg(&path)
                .exec())
            .with_context(|| format!("failed to execute bat for file {}", path.display()))?;
        }
        Err(process::Command::new("file")
            .arg("--brief")
            .arg("--dereference")
            .arg(&path)
            .exec())
        .with_context(|| format!("failed to execute file command for {}", path.display()))?;
    }

    let out = process::Command::new("git")
        .arg("branch")
        .arg("--list")
        .arg(&conf.input)
        .arg("--format=%(refname:short)")
        .output()
        .context("failed to execute git branch command")?;
    let stdout = out.stdout.trim_ascii();
    let is_git_branch = stdout == conf.input.as_bytes();
    if is_git_branch && conf.input != "main" && conf.input != "master" {
        Err(process::Command::new("git")
            .arg("log")
            .arg("--color=always")
            .arg("--oneline")
            .arg(format!("main..{}", conf.input))
            .arg(&path)
            .exec())
        .with_context(|| format!("failed to execute git log for branch {}", conf.input))?;
    }

    Ok(())
}
