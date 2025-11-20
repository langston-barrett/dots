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
    skel: PathBuf,
    fragments: PathBuf,
}

fn get_git_files() -> Result<Vec<PathBuf>> {
    let output = process::Command::new("git")
        .args(["ls-files", "--exclude-standard"])
        .output()
        .context("failed to execute `git ls-files`")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("`git ls-files` failed: {stdout}\n{stderr}");
    }

    Ok(stdout.lines().map(PathBuf::from).collect())
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
            debug!("recorded fragment from {}", path.display());
            files.insert(relative_path, content);
        } else {
            warn!("not a file or directory: {}", path.display());
        }
    }
    Ok(())
}

fn collect_files(dir: PathBuf) -> Result<HashMap<PathBuf, String>> {
    let mut files = HashMap::with_capacity(32);
    walk_dir(&dir, &dir, &mut files)?;
    Ok(files)
}

fn collect_fragments(fragments_dir: &Path) -> Result<HashMap<String, Vec<String>>> {
    let mut fragments = HashMap::new();

    for entry in fs::read_dir(fragments_dir).with_context(|| {
        format!(
            "failed to read fragments directory: {}",
            fragments_dir.display()
        )
    })? {
        let entry = entry.with_context(|| {
            format!(
                "failed to read entry in fragments directory: {}",
                fragments_dir.display()
            )
        })?;
        let path = entry.path();

        if path.is_file() {
            let content = fs::read_to_string(&path)
                .with_context(|| format!("couldn't read fragment: {}", path.display()))?;

            let lines: Vec<String> = content.lines().map(String::from).collect();
            if let Some(first_line) = lines.first() {
                let first_line = first_line.clone();
                fragments.insert(first_line.clone(), lines);
                debug!("recorded fragment with first line: {}", first_line);
            } else {
                warn!("empty fragment file: {}", path.display());
            }
        }
    }

    Ok(fragments)
}

fn replace_matching_files(files: &HashMap<PathBuf, String>, git_files: &[PathBuf]) -> Result<()> {
    for path in git_files {
        if let Some(content) = files.get(path) {
            fs::write(path, content)
                .with_context(|| format!("failed to write to {}", path.display()))?;
            debug!("replaced {}", path.display());
        }
    }

    Ok(())
}

fn replace_fragment_matches(
    fragments: &HashMap<String, Vec<String>>,
    git_files: &[PathBuf],
) -> Result<()> {
    for path in git_files {
        let content = match fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                debug!("couldn't read {}: {}", path.display(), e);
                continue;
            }
        };

        let mut lines = Vec::with_capacity(content.len() / 80);
        let mut modified = false;
        let mut skip = false;

        'outer: for line in content.lines() {
            if skip {
                if line.is_empty() {
                    skip = false;
                }
                continue;
            }

            for (first_line, fragment_lines) in fragments {
                if line.trim() == first_line.trim() {
                    lines.extend(fragment_lines.iter().cloned());
                    skip = true;
                    modified = true;
                    debug!("replaced fragment match in {}", path.display());
                    continue 'outer;
                }
            }
            lines.push(line.to_owned());
        }

        if modified {
            let new_content = lines.join("\n");
            fs::write(path, new_content)
                .with_context(|| format!("failed to write to {}", path.display()))?;
        }
    }

    Ok(())
}

pub(super) fn go(config: Config) -> Result<(), Box<dyn Error>> {
    let git_files = get_git_files()?;

    let files = collect_files(config.skel)?;
    replace_matching_files(&files, &git_files)?;

    let fragments = collect_fragments(&config.fragments)?;
    replace_fragment_matches(&fragments, &git_files)?;

    Ok(())
}
