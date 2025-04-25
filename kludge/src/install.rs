use std::{env, error::Error, fs, os::unix::fs::symlink, path::PathBuf};

pub(super) fn go() -> Result<(), Box<dyn Error>> {
    let home = PathBuf::from(env::var("HOME")?);
    let config_home = PathBuf::from(env::var("XDG_CONFIG_HOME")?);
    let here = env::current_dir()?;
    for (src, dst) in [
        ("tmux", config_home.join("tmux")),
        ("tmux/tmux.conf", home.join(".tmux.conf")),
        ("qutebrowser", config_home.join("qutebrowser")),
        ("qutebrowser", home.join(".qutebrowser")),
    ] {
        if dst.is_symlink() {
            fs::remove_file(&dst)?;
        }
        symlink(here.join("files").join(src), dst)?;
    }
    Ok(())
}
