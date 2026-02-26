use anyhow::Context as _;
use std::{
    env, fs,
    os::unix::fs::symlink,
    path::{Path, PathBuf},
};

fn get_mappings(
    home: &Path,
    config_home: &Path,
    local_share: &Path,
) -> [(&'static str, PathBuf); 31] {
    [
        ("aspell-dict", home.join(".aspell.en.pws")),
        ("bash", config_home.join("bash")),
        ("bashrc", home.join(".bashrc")),
        ("claude", home.join("claude")),
        ("config.nix", config_home.join("nixpkgs/config.nix")),
        ("git/gitconfig", home.join(".gitconfig")),
        ("git/gitignore", config_home.join("git/ignore")),
        ("home.nix", config_home.join("home-manager/home.nix")),
        ("i3", config_home.join("i3")),
        ("inputrc", home.join(".inputrc")),
        ("kmonad", config_home.join("kmonad")),
        ("nix-channels", home.join(".nix-channels")),
        ("nix.conf", config_home.join("nix/nix.conf")),
        ("polybar.ini", config_home.join("polybar/config.ini")),
        ("qutebrowser", config_home.join("qutebrowser")),
        ("scripts", home.join("scripts")),
        ("sh.d", config_home.join("sh.d")),
        ("ssh", home.join(".ssh/config")),
        ("taskrc", config_home.join("task/taskrc")),
        ("tmux", config_home.join("tmux")),
        ("tmux/tmux.conf", home.join(".tmux.conf")),
        ("unsplash", local_share.join("wallpapers/unsplash")),
        ("Xresources", home.join(".Xresources")),
        ("zsh", config_home.join("zsh")),
        ("zshrc", home.join(".zshrc")),
        (
            "alacritty.toml",
            config_home.join("alacritty/alacritty.toml"),
        ),
        (
            "desktop/anki.desktop",
            local_share.join("applications/anki.desktop"),
        ),
        (
            "desktop/helix.desktop",
            local_share.join("applications/helix.desktop"),
        ),
        (
            "desktop/spacemacs.desktop",
            local_share.join("applications/spacemacs.desktop"),
        ),
        (
            "desktop/spotify.desktop",
            local_share.join("applications/spotify.desktop"),
        ),
        ("envrcs", config_home.join("envrcs")),
    ]
}

pub(super) fn go() -> anyhow::Result<()> {
    let home = PathBuf::from(env::var("HOME").context("HOME environment variable not set")?);
    let config_home =
        PathBuf::from(env::var("XDG_CONFIG_HOME").context("XDG_CONFIG_HOME not set")?);
    let local_share = home.join(".local/share");
    let here = env::current_dir().context("failed to get current directory")?;

    for (src, dst) in get_mappings(&home, &config_home, &local_share) {
        let parent = dst
            .parent()
            .with_context(|| format!("no parent for {}", dst.display()))?;
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create parent directory for {}", dst.display()))?;

        let expected_target = here.join("files").join(src);

        if dst.is_symlink() {
            if let Ok(current_target) = fs::read_link(&dst)
                && current_target == expected_target
            {
                continue;
            }
            fs::remove_file(&dst).with_context(|| {
                format!("failed to remove incorrect symlink at {}", dst.display())
            })?;
        }

        symlink(&expected_target, &dst).with_context(|| {
            format!("failed to create symlink from {} to {}", src, dst.display())
        })?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::fs;
    use std::path::{Path, PathBuf};

    /// Top-level files/directories that don't need symlinking
    fn get_excluded_paths() -> HashSet<String> {
        let mut excluded = HashSet::new();

        // Icon referenced directly by desktop files with absolute path
        excluded.insert("anki.png".to_string());
        // xres subdirectories are included by Xresources with absolute paths
        excluded.insert("xres".to_string());
        // nix subdirectories are imported/included by other nix configs
        excluded.insert("nix".to_string());

        excluded
    }

    #[test]
    fn all_files_have_destinations() {
        // Read files/ directory dynamically
        let files_dir = Path::new("files");
        let files_dir = if !files_dir.exists() {
            // If running from kludge subdirectory, adjust path
            Path::new("../files")
        } else {
            files_dir
        };

        let mut all_files = HashSet::new();
        if let Ok(dir_entries) = fs::read_dir(files_dir) {
            for entry in dir_entries.flatten() {
                let path = entry.path();
                // Skip attic directory
                if path.file_name().and_then(|n| n.to_str()) == Some("attic") {
                    continue;
                }
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    all_files.insert(name.to_string());
                }
            }
        }

        // Extract mapped sources from actual mappings
        let home = PathBuf::from("/tmp");
        let config_home = PathBuf::from("/tmp");
        let local_share = PathBuf::from("/tmp");
        let mappings = super::get_mappings(&home, &config_home, &local_share);

        let mut mapped = HashSet::new();
        for (src, _) in mappings {
            // Extract top-level path from source
            let top_level = src.split('/').next().unwrap();
            mapped.insert(top_level.to_string());
        }

        let excluded = get_excluded_paths();

        let mut unmapped = Vec::new();

        for file in &all_files {
            if mapped.contains(file) {
                continue;
            }
            if excluded.contains(file) {
                continue;
            }
            unmapped.push(file.clone());
        }

        if !unmapped.is_empty() {
            unmapped.sort();
            panic!(
                "The following files in files/ directory do not have symlink destinations defined:\n  {}",
                unmapped.join("\n  ")
            );
        }
    }
}
