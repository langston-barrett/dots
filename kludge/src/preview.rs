use std::{
    error::Error, fs, io::Read as _, os::unix::process::CommandExt as _, path::PathBuf, process,
};

/// Preview a file (e.g., for fzf)
#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    input: String,
}

// https://github.com/Aloxaf/fzf-tab/wiki/Preview#show-file-contents
pub(super) fn go(conf: Config) -> Result<(), Box<dyn Error>> {
    let path = PathBuf::from(&conf.input);
    if path.is_dir() {
        Err(process::Command::new("ls")
            .arg("--color=always")
            .arg(&path)
            .exec())?;
    }

    if let Ok(true) = path.try_exists() {
        let bs = {
            let mut f = fs::File::open(&path)?;
            let mut buf = vec![0u8; 16];
            let read = f.read(&mut buf)?;
            buf.truncate(read);
            buf
        };
        let is_ascii = bs.iter().all(|&b| b <= 0x7F);
        if is_ascii {
            Err(process::Command::new("bat")
                .arg("--color=always")
                .arg("--plain")
                .arg(&path)
                .exec())?;
        }
        Err(process::Command::new("file")
            .arg("--brief")
            .arg("--dereference")
            .arg(&path)
            .exec())?;
    }

    let out = process::Command::new("git")
        .arg("branch")
        .arg("--list")
        .arg(&conf.input)
        .arg("--format=%(refname:short)")
        .output()?;
    let stdout = out.stdout.trim_ascii();
    let is_git_branch = stdout == conf.input.as_bytes();
    if is_git_branch && conf.input != "main" && conf.input != "master" {
        Err(process::Command::new("git")
            .arg("log")
            .arg("--color=always")
            .arg("--oneline")
            .arg(format!("main..{}", conf.input))
            .arg(&path)
            .exec())?;
    }

    Ok(())
}
