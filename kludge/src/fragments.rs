use std::{
    collections::HashMap,
    error::Error,
    fs,
    path::{Path, PathBuf},
    process,
};

use anyhow::{Context as _, Result, bail};
use tracing::{debug, warn};

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    paths: Vec<PathBuf>,
}

fn walk_dir(dir: &Path, root: &Path, files: &mut HashMap<PathBuf, String>) -> Result<()> {
    for entry in
        fs::read_dir(dir).with_context(|| format!("failed to read directory: {}", dir.display()))?
    {
        let entry = entry
            .with_context(|| format!("failed to read entry in directory: {}", dir.display()))?;
        let path = entry.path();

        if path.is_dir() {
            walk_dir(&path, root, files)?;
        } else if path.is_file() {
            let relative_path = path
                .strip_prefix(root)
                .with_context(|| format!("failed to get relative path: {}", path.display()))?
                .to_path_buf();
            let content = fs::read_to_string(path.as_path())
                .with_context(|| format!("couldn't read {}", path.display()))?;
            debug!("Recorded fragment from {}", path.display());
            files.insert(relative_path, content);
        } else {
            warn!("not a file or directory: {}", path.display());
        }
    }
    Ok(())
}

fn collect_files(fragments: Vec<PathBuf>) -> Result<HashMap<PathBuf, String>> {
    let mut files = HashMap::with_capacity(32);
    for dir in fragments {
        walk_dir(&dir, &dir, &mut files)?;
    }
    Ok(files)
}

fn replace_matching_files(files: &HashMap<PathBuf, String>) -> Result<()> {
    let output = process::Command::new("git")
        .args(["ls-files", "--exclude-standard"])
        .output()
        .context("failed to execute `git ls-files`")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("`git ls-files` failed: {stdout}\n{stderr}");
    }

    for git_file in stdout.lines() {
        let git_path = PathBuf::from(git_file);
        if let Some(content) = files.get(&git_path) {
            fs::write(&git_path, content)
                .with_context(|| format!("failed to write to {}", git_path.display()))?;
            debug!("replaced {}", git_path.display());
        }
    }

    Ok(())
}

pub(super) fn go(config: Config) -> Result<(), Box<dyn Error>> {
    let files = collect_files(config.paths)?;
    replace_matching_files(&files)?;
    Ok(())
}
