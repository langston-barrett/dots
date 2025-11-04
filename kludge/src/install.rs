use anyhow::Context;
use std::{env, error::Error, os::unix::fs::symlink, path::PathBuf};

pub(super) fn go() -> Result<(), Box<dyn Error>> {
    let home = PathBuf::from(env::var("HOME")?);
    let config_home =
        PathBuf::from(env::var("XDG_CONFIG_HOME").context("XDG_CONFIG_HOME not set")?);
    let here = env::current_dir()?;
    for (src, dst) in [
        ("tmux", config_home.join("tmux")),
        ("tmux/tmux.conf", home.join(".tmux.conf")),
        ("qutebrowser", config_home.join("qutebrowser")),
        ("qutebrowser", home.join(".qutebrowser")),
        ("zshrc", home.join(".zshrc")),
        ("zsh.d", home.join(".zsh.d")),
        ("sh.d", home.join(".sh.d")),
    ] {
        if dst.is_symlink() {
            continue;
            // fs::remove_file(&dst)?;
        }
        symlink(here.join("files").join(src), dst)?;
    }
    Ok(())
}
