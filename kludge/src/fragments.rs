use std::{
    collections::HashMap,
    env,
    ffi::OsString,
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

fn get_git_root_name() -> Result<String> {
    let current_dir = env::current_dir().context("failed to get current directory")?;
    for dir in current_dir.ancestors() {
        if dir.join(".git").is_dir() {
            return dir
                .file_name()
                .and_then(|n| n.to_str())
                .map(ToString::to_string)
                .with_context(|| format!("failed to get directory name from {}", dir.display()));
        }
    }
    bail!("could not find git root directory")
}

fn replace_name_placeholder(content: &str, name: &str) -> String {
    content.replace("{{name}}", name)
}

fn walk_dir(
    dir: &Path,
    root: &Path,
    files: &mut HashMap<PathBuf, String>,
    name: &str,
) -> Result<()> {
    for entry in
        fs::read_dir(dir).with_context(|| format!("failed to read directory: {}", dir.display()))?
    {
        let entry = entry
            .with_context(|| format!("failed to read entry in directory: {}", dir.display()))?;
        let path = entry.path();

        if path.is_dir() {
            walk_dir(&path, root, files, name)
                .with_context(|| format!("failed to walk directory {}", path.display()))?;
        } else if path.is_file() {
            let relative_path = path
                .strip_prefix(root)
                .with_context(|| format!("failed to get relative path: {}", path.display()))?
                .to_path_buf();
            let content = fs::read_to_string(path.as_path())
                .with_context(|| format!("couldn't read {}", path.display()))?;
            let content = replace_name_placeholder(&content, name);
            debug!("recorded fragment from {}", path.display());
            files.insert(relative_path, content);
        } else {
            warn!("not a file or directory: {}", path.display());
        }
    }
    Ok(())
}

fn collect_files(dir: PathBuf, name: &str) -> Result<HashMap<PathBuf, String>> {
    let mut files = HashMap::with_capacity(32);
    walk_dir(&dir, &dir, &mut files, name)?;
    Ok(files)
}

struct Fragment {
    lines: Vec<String>,
    extension: Option<OsString>,
}

fn collect_fragments(fragments_dir: &Path, name: &str) -> Result<HashMap<String, Fragment>> {
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
            let content = replace_name_placeholder(&content, name);

            let lines: Vec<String> = content.lines().map(String::from).collect();
            if let Some(first_line) = lines.first() {
                let first_line = first_line.clone();
                let extension = path.extension().map(std::ffi::OsStr::to_os_string);
                fragments.insert(first_line.clone(), Fragment { lines, extension });
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
    fragments: &HashMap<String, Fragment>,
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

            for (first_line, fragment) in fragments {
                if fragment.extension.as_deref() != path.extension() {
                    continue;
                }

                if line.trim() == first_line.trim() {
                    lines.extend(fragment.lines.iter().cloned());
                    lines.push(String::new());
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

pub(super) fn go(config: Config) -> Result<()> {
    let git_files = get_git_files()?;
    let git_root_name = get_git_root_name().context("failed to get git root name")?;

    let skel = config.skel.clone();
    let files = collect_files(config.skel, &git_root_name)
        .with_context(|| format!("failed to collect files from {}", skel.display()))?;
    replace_matching_files(&files, &git_files).context("failed to replace matching files")?;

    let fragments_path = config.fragments.clone();
    let fragments = collect_fragments(&config.fragments, &git_root_name).with_context(|| {
        format!(
            "failed to collect fragments from {}",
            fragments_path.display()
        )
    })?;
    replace_fragment_matches(&fragments, &git_files)
        .context("failed to replace fragment matches")?;

    Ok(())
}
