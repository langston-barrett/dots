use std::{
    env, fs,
    io::{Seek, Write},
    os::unix::{ffi::OsStrExt, process::CommandExt as _},
    process,
};

use anyhow::Context as _;

const DELIM: &str = "\t";

fn apps(stdin: &mut fs::File) -> anyhow::Result<()> {
    if let Ok(it) = fs::read_dir("/Applications") {
        for entry in it {
            let entry = entry?;
            let name = entry.path().with_extension("");
            let name = name.file_name();
            let path = entry.path();
            let name = name.unwrap_or(path.as_os_str());
            if name.as_bytes().first() == Some(&b'.') {
                continue;
            }
            stdin.write_all(name.as_bytes())?;
            stdin.write_all(DELIM.as_bytes())?;
            stdin.write_all(b"open -a '")?;
            stdin.write_all(entry.path().as_os_str().as_bytes())?;
            stdin.write_all(b"'")?;
            stdin.write_all(b"\n")?;
        }
    }
    Ok(())
}

pub(super) fn go() -> anyhow::Result<()> {
    unsafe { env::remove_var("ITERM_PROFILE") };

    let p = "/Users/langston/.launcher";
    if let Ok(stdin) = fs::File::open(p) {
        Err(process::Command::new("pick")
            .stdin(stdin)
            .arg(format!("--delimiter={DELIM}"))
            .arg("--with-nth={1}")
            .arg("--bind=enter:become(zsh -ic {2})")
            .arg("--preview-window=hidden")
            .exec())?;
    }
    let mut stdin = fs::File::create(p).with_context(|| format!("Failed to create {p}"))?;

    apps(&mut stdin)?;

    stdin.write_all("Tasks".as_bytes())?;
    stdin.write_all(DELIM.as_bytes())?;
    stdin.write_all("/Users/langston/code/dots/files/scripts/bin/tasks".as_bytes())?;

    stdin.flush()?;
    stdin.rewind()?;
    let stdin = fs::File::open(p)?;
    Err(process::Command::new("pick")
        .stdin(stdin)
        .arg(format!("--delimiter={DELIM}"))
        .arg("--with-nth={1}")
        .arg("--bind=enter:become(zsh -ic {2})")
        .arg("--preview-window=hidden")
        .exec())?
}
