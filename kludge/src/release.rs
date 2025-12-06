use std::ffi::OsStr;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context as _, bail};
use tracing::{debug, info, warn};

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[clap(short = 'i', long)]
    initial: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Version {
    major: u64,
    minor: u64,
    patch: u64,
}

impl Version {
    fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    fn parse(s: &str) -> anyhow::Result<Self> {
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            anyhow::bail!("version must have exactly 3 parts separated by dots");
        }
        let major = parts[0]
            .parse::<u64>()
            .with_context(|| format!("invalid major version: {}", parts[0]))?;
        let minor = parts[1]
            .parse::<u64>()
            .with_context(|| format!("invalid minor version: {}", parts[1]))?;
        let patch = parts[2]
            .parse::<u64>()
            .with_context(|| format!("invalid patch version: {}", parts[2]))?;
        Ok(Self {
            major,
            minor,
            patch,
        })
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

#[derive(Debug, Clone, Copy)]
enum VersionBump {
    Major,
    Minor,
    Patch,
}

fn git_exec(args: &[&str]) -> anyhow::Result<()> {
    let status = Command::new("git")
        .args(args)
        .status()
        .with_context(|| format!("failed to execute `git {args:?}`"))?;
    if !status.success() {
        anyhow::bail!("git command failed: `git {args:?}`");
    }
    Ok(())
}

fn git(args: &[&str]) -> anyhow::Result<String> {
    let output = Command::new("git")
        .args(args)
        .output()
        .with_context(|| format!("failed to execute `git {args:?}`"))?;
    if !output.status.success() {
        anyhow::bail!("git command failed: `git {args:?}`");
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn prompt_user() -> anyhow::Result<VersionBump> {
    loop {
        info!("Bump version: (m)ajor, m(i)nor, (p)atch: ");
        io::stderr().flush().context("failed to flush stderr")?;
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .context("failed to read from stdin")?;
        let trimmed = input.trim().to_lowercase();
        match trimmed.as_str() {
            "m" | "major" => return Ok(VersionBump::Major),
            "i" | "minor" => return Ok(VersionBump::Minor),
            "p" | "patch" => return Ok(VersionBump::Patch),
            _ => warn!("Invalid choice. Please enter '(m)ajor', 'm(i)nor', or '(p)atch'."),
        }
    }
}

fn latest() -> anyhow::Result<Version> {
    let args: &[&str] = &["tag", "--sort=-version:refname"];
    let out = git(args)?;
    for tag in out.lines() {
        let version_str = tag.strip_prefix('v').unwrap_or(tag);
        if let Ok(version) = Version::parse(version_str) {
            return Ok(version);
        }
    }
    Ok(Version::default())
}

fn bump(current: &Version, bump_type: VersionBump) -> Version {
    match bump_type {
        VersionBump::Major => Version::new(current.major + 1, 0, 0),
        VersionBump::Minor => Version::new(current.major, current.minor + 1, 0),
        VersionBump::Patch => Version::new(current.major, current.minor, current.patch + 1),
    }
}

fn find_git_root() -> anyhow::Result<PathBuf> {
    let output = git(&["rev-parse", "--show-toplevel"])?;
    Ok(PathBuf::from(output))
}

fn get_repo_name(git_root: &Path) -> anyhow::Result<String> {
    let dir_name = git_root
        .file_name()
        .and_then(|n| n.to_str())
        .context("failed to get directory name")?;
    Ok(dir_name.to_string())
}

fn update_changelog(path: &Path, new_version: &Version, repo_name: &str) -> anyhow::Result<()> {
    assert_eq!(path.extension(), Some(OsStr::new(".md")));
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read changelog: {}", path.display()))?;
    let new_content = update_changelog_str(new_version, repo_name, content)?;
    std::fs::write(path, new_content)
        .with_context(|| format!("failed to write changelog: {}", path.display()))?;
    git_exec(&[
        "add",
        path.as_os_str().to_string_lossy().into_owned().as_str(),
    ])?;
    Ok(())
}

fn update_changelog_str(
    new_version: &Version,
    repo_name: &str,
    content: String,
) -> Result<String, anyhow::Error> {
    let next_header = "## next";
    let Some(next_start) = content.find(next_header) else {
        warn!("no `## next` header in CHANGELOG, skipping update");
        return Ok(content);
    };
    let after_next = &content[next_start + next_header.len()..];
    let next_end = after_next
        .find("\n## ")
        .map_or(content.len(), |i| next_start + next_header.len() + i);
    let next_entries = content[next_start + next_header.len()..next_end].trim();
    if next_entries.is_empty() {
        anyhow::bail!("No entries found under '## next' section");
    }
    let date = Command::new("date")
        .arg("+%Y-%m-%d")
        .output()
        .context("failed to execute date command")?
        .stdout;
    let date = String::from_utf8_lossy(&date).trim().to_string();
    let version_link = format!(
        "[{new_version}]: https://github.com/langston-barrett/{repo_name}/releases/tag/v{new_version}"
    );
    let new_section = format!("## [{new_version}] - {date}\n\n{version_link}\n\n{next_entries}\n");
    let changelog_header = "# CHANGELOG";
    let header_pos = content.find(changelog_header);
    let mut new_content = String::new();
    let remaining_content = if let Some(header_start) = header_pos {
        let header_end = header_start + changelog_header.len();
        new_content.push_str(&content[..header_end]);
        new_content.push_str(&content[header_end..next_start]);
        new_content.push_str(&new_section);
        &content[next_end..]
    } else {
        new_content.push_str(&content[..next_start]);
        new_content.push_str(&new_section);
        &content[next_end..]
    };
    new_content.push_str(remaining_content);
    Ok(new_content)
}

fn get_cargo_toml_version(content: &str) -> anyhow::Result<Version> {
    let package_start = content
        .find("[package]")
        .context("could not find [package] section in Cargo.toml")?;
    let after_package = &content[package_start..];
    let next_section = after_package
        .find("\n[")
        .map_or(content.len(), |i| package_start + i);
    let package_section = &content[package_start..next_section];
    let version_line = package_section
        .lines()
        .find(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("version") && trimmed.contains('=')
        })
        .context("could not find version field in Cargo.toml")?;
    let equals_pos = version_line.find('=').context("version line missing '='")?;
    let after_equals_raw = &version_line[equals_pos + 1..];
    let after_equals_trimmed = after_equals_raw.trim_start();
    let quote_start_in_trimmed = after_equals_trimmed
        .find('"')
        .context("version value missing opening quote")?;
    let value_end_in_trimmed = after_equals_trimmed[quote_start_in_trimmed + 1..]
        .find('"')
        .context("version value missing closing quote")?;
    let version_str = &after_equals_trimmed
        [quote_start_in_trimmed + 1..quote_start_in_trimmed + 1 + value_end_in_trimmed];
    Version::parse(version_str)
}

fn get_all_cargo_toml_files(git_root: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let output = git(&["ls-files", "--exclude-standard"])?;
    let mut cargo_toml_files = Vec::new();
    for line in output.lines() {
        if line.ends_with("Cargo.toml") {
            cargo_toml_files.push(git_root.join(line));
        }
    }
    Ok(cargo_toml_files)
}

fn update_cargo_toml(path: &Path, new_version: &Version) -> anyhow::Result<()> {
    assert!(path.ends_with("Cargo.toml"));
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read Cargo.toml: {}", path.display()))?;
    let new_content = update_cargo_toml_str(new_version, content)?;
    std::fs::write(path, new_content)
        .with_context(|| format!("failed to write Cargo.toml: {}", path.display()))?;
    git_exec(&[
        "add",
        path.as_os_str().to_string_lossy().into_owned().as_str(),
    ])?;
    Ok(())
}

fn update_cargo_toml_str(new_version: &Version, content: String) -> Result<String, anyhow::Error> {
    let package_start = content
        .find("[package]")
        .context("could not find [package] section in Cargo.toml")?;
    let after_package = &content[package_start..];
    let next_section = after_package
        .find("\n[")
        .map_or(content.len(), |i| package_start + i);
    let package_section = &content[package_start..next_section];
    let version_line = package_section
        .lines()
        .find(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("version") && trimmed.contains('=')
        })
        .context("could not find version field in Cargo.toml")?;
    let version_line_start = content[..next_section]
        .find(version_line)
        .context("could not locate version line")?;
    let version_line_end = version_line_start + version_line.len();
    let equals_pos = version_line.find('=').context("version line missing '='")?;
    let after_equals_raw = &version_line[equals_pos + 1..];
    let after_equals_trimmed = after_equals_raw.trim_start();
    let trim_offset = after_equals_raw.len() - after_equals_trimmed.len();
    let quote_start_in_trimmed = after_equals_trimmed
        .find('"')
        .context("version value missing opening quote")?;
    let quote_start_in_line = equals_pos + 1 + trim_offset + quote_start_in_trimmed;
    let value_end_in_trimmed = after_equals_trimmed[quote_start_in_trimmed + 1..]
        .find('"')
        .context("version value missing closing quote")?;
    let value_end_in_line =
        equals_pos + 1 + trim_offset + quote_start_in_trimmed + 1 + value_end_in_trimmed;
    let new_version_line = format!(
        "{}\"{}\"{}",
        &version_line[..quote_start_in_line],
        new_version,
        &version_line[value_end_in_line + 1..]
    );
    let mut new_content = String::new();
    new_content.push_str(&content[..version_line_start]);
    new_content.push_str(&new_version_line);
    new_content.push_str(&content[version_line_end..]);
    Ok(new_content)
}

fn update_cargo_tomls(
    current_version: Version,
    new_version: Version,
    git_root: PathBuf,
) -> Result<(), anyhow::Error> {
    debug!("Updating Cargo.toml files");
    let cargo_toml_files = get_all_cargo_toml_files(&git_root)?;
    for path in &cargo_toml_files {
        if !path.exists() {
            warn!("No Cargo.toml at {}", path.display());
            continue;
        }
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read Cargo.toml: {}", path.display()))?;
        match get_cargo_toml_version(&content) {
            Ok(version) if version == current_version => {
                info!("Updating version in {}", path.display());
                update_cargo_toml(path, &new_version)?;
            }
            Ok(version) => {
                info!(
                    "Skipping {} (version {} != {})",
                    path.display(),
                    version,
                    current_version
                );
            }
            Err(e) => {
                info!(
                    "Skipping {} (could not parse version: {})",
                    path.display(),
                    e
                );
            }
        }
    }
    Ok(())
}

fn run_cargo_clippy() -> anyhow::Result<()> {
    if !Path::new("Cargo.toml").exists() {
        debug!("No Cargo.toml, skipping clippy");
        return Ok(());
    }
    debug!("Running clippy");
    let status = Command::new("cargo")
        .args(["clippy", "--all-targets", "--", "--deny", "warnings"])
        .status()
        .context("failed to execute cargo clippy")?;
    if !status.success() {
        anyhow::bail!("cargo clippy failed");
    }
    git_exec(&["add", "Cargo.lock"])?;
    Ok(())
}

fn bump_changelog(new_version: Version, git_root: &Path) -> Result<(), anyhow::Error> {
    debug!("Updating CHANGELOG.md");
    let repo_name = get_repo_name(git_root)?;
    let changelog_path = git_root.join("CHANGELOG.md");
    if changelog_path.exists() {
        update_changelog(&changelog_path, &new_version, &repo_name)?;
    } else {
        info!("No CHANGELOG, skipping");
    }
    Ok(())
}

pub(super) fn go(conf: Config) -> anyhow::Result<()> {
    git_exec(&["checkout", "main"])?;
    git_exec(&["pull", "origin", "main"])?;
    drop(git_exec(&["branch", "-D", "release"]));
    git_exec(&["checkout", "-b", "release"])?;

    let bump_type = prompt_user()?;
    let current_version = latest()?;
    let initial = current_version != Version::default();
    if conf.initial && !initial {
        bail!("--initial specified, but current version is {initial}");
    }
    let new_version = if initial {
        Version::new(0, 1, 0)
    } else {
        bump(&current_version, bump_type)
    };

    info!("Current version: {current_version}");
    info!("New version: {new_version}");

    let git_root = find_git_root()?;
    if initial {
        if !Path::new("CHANGELOG.md").exists() {
            bail!("Please create CHANGELOG.md");
        }
    } else {
        bump_changelog(new_version, &git_root)?;
    }

    update_cargo_tomls(current_version, new_version, git_root)?;
    run_cargo_clippy()?;
    let v = format!("v{new_version})");
    git_exec(&["commit", "-m", &v])?;
    git_exec(&["push"])?;

    info!("Now wait for CI...");
    io::stderr().flush().context("failed to flush stderr")?;
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .context("failed to read from stdin")?;

    git_exec(&["checkout", "main"])?;
    git_exec(&["pull", "origin", "main"])?;
    git_exec(&["tag", "-a", &v, "-m", &v])?;
    git_exec(&["push", "--tags"])?;

    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn update_cargo_version() {
        let content = r#"[package]
name = "test"
description = "Test package"
version = "0.0.0"
edition = "2024"
authors = ["Test Author"]

[dependencies]
anyhow = "1"
clap = { version = "4", features = ["derive"] }
"#;
        let new_version = Version::parse("1.2.3").unwrap();
        let result = update_cargo_toml_str(&new_version, content.to_string()).unwrap();
        expect![[r#"[package]
name = "test"
description = "Test package"
version = "1.2.3"
edition = "2024"
authors = ["Test Author"]

[dependencies]
anyhow = "1"
clap = { version = "4", features = ["derive"] }
"#]]
        .assert_eq(&result);
    }

    #[test]
    fn update_changelog() {
        let before = r#"# CHANGELOG

## next

- Added new feature
- Fixed bug

## [0.1.0] - 2024-01-01

[0.1.0]: https://github.com/langston-barrett/test/releases/tag/v0.1.0

- Initial release
"#;

        let new_version = Version::parse("0.2.0").unwrap();
        let repo_name = "test";
        let result = update_changelog_str(&new_version, repo_name, before.to_string()).unwrap();

        // Normalize date to placeholder for comparison
        let date_start = result.find("## [0.2.0] - ").unwrap() + 13;
        let date_end = result[date_start..].find('\n').unwrap() + date_start;
        let normalized = format!(
            "{}{}{}",
            &result[..date_start],
            "YYYY-MM-DD",
            &result[date_end..]
        );

        expect![[r#"# CHANGELOG

## [0.2.0] - YYYY-MM-DD

[0.2.0]: https://github.com/langston-barrett/test/releases/tag/v0.2.0

- Added new feature
- Fixed bug

## [0.1.0] - 2024-01-01

[0.1.0]: https://github.com/langston-barrett/test/releases/tag/v0.1.0

- Initial release
"#]]
        .assert_eq(&normalized);
    }
}
