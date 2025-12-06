use anyhow::Context as _;
use std::{env, fs, os::unix::fs::symlink, path::PathBuf};

pub(super) fn go() -> anyhow::Result<()> {
    let home = PathBuf::from(env::var("HOME").context("HOME environment variable not set")?);
    let config_home =
        PathBuf::from(env::var("XDG_CONFIG_HOME").context("XDG_CONFIG_HOME not set")?);
    let here = env::current_dir().context("failed to get current directory")?;
    for (src, dst) in [
        ("inputrc", home.join(".inputrc")),
        ("polybar.ini", config_home.join("polybar/config.ini")),
        ("qutebrowser", config_home.join("qutebrowser")),
        ("qutebrowser", home.join(".qutebrowser")),
        ("sh.d", home.join(".sh.d")),
        ("taskrc", config_home.join("task/taskrc")),
        ("tmux", config_home.join("tmux")),
        ("tmux/tmux.conf", home.join(".tmux.conf")),
        ("zsh.d", home.join(".zsh.d")),
        ("zshrc", home.join(".zshrc")),
    ] {
        fs::create_dir_all(dst.parent().unwrap())
            .with_context(|| format!("failed to create parent directory for {}", dst.display()))?;
        if dst.is_symlink() {
            continue;
            // fs::remove_file(&dst)?;
        }
        symlink(here.join("files").join(src), &dst).with_context(|| {
            format!("failed to create symlink from {} to {}", src, dst.display())
        })?;
    }
    Ok(())
}
