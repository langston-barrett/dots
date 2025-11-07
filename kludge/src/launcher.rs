use std::{
    fs,
    io::{Seek, Write},
    os::unix::{ffi::OsStrExt, process::CommandExt as _},
    process,
};

use anyhow::Context as _;

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
            stdin.write_all(b";")?;
            stdin.write_all(b"open -a '")?;
            stdin.write_all(entry.path().as_os_str().as_bytes())?;
            stdin.write_all(b"'")?;
            stdin.write_all(b"\n")?;
        }
    }
    Ok(())
}

pub(super) fn go() -> anyhow::Result<()> {
    let p = "/Users/langston/.launcher";
    if let Ok(stdin) = fs::File::open(p) {
        Err(process::Command::new("pick")
            .stdin(stdin)
            .arg("--delimiter=;")
            .arg("--with-nth={1}")
            .arg("--bind=enter:become(zsh -c {2})")
            .arg("--preview-window=hidden")
            .exec())?;
    }
    let mut stdin = fs::File::create(p).with_context(|| format!("Failed to create {p}"))?;

    apps(&mut stdin)?;

    stdin.flush()?;
    stdin.rewind()?;
    let stdin = fs::File::open(p)?;
    Err(process::Command::new("pick")
        .stdin(stdin)
        .arg("--delimiter=;")
        .arg("--with-nth={1}")
        .arg("--bind=enter:become(zsh -c {2})")
        .arg("--preview-window=hidden")
        .exec())?
}
