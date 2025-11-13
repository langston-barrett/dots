use anyhow::Context;
use std::{env, error::Error, fs, os::unix::fs::symlink, path::PathBuf};

pub(super) fn go() -> Result<(), Box<dyn Error>> {
    let home = PathBuf::from(env::var("HOME")?);
    let config_home =
        PathBuf::from(env::var("XDG_CONFIG_HOME").context("XDG_CONFIG_HOME not set")?);
    let here = env::current_dir()?;
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
        fs::create_dir_all(dst.parent().unwrap())?;
        if dst.is_symlink() {
            continue;
            // fs::remove_file(&dst)?;
        }
        symlink(here.join("files").join(src), dst)?;
    }
    Ok(())
}
