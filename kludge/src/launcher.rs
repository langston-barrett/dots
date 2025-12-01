use std::{
    env, fs,
    io::{Seek, Write},
    os::unix::{ffi::OsStrExt, process::CommandExt as _},
    process,
};

use anyhow::Context as _;

const DELIM: &str = "\t";

fn apps(stdin: &mut fs::File) -> anyhow::Result<()> {
    #[cfg(target_os = "macos")]
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

    #[cfg(target_os = "linux")]
    {
        let path_var = env::var("PATH").expect("PATH not found");
        let mut paths = path_var.split(':').collect::<Vec<_>>();
        paths.dedup();
        let mut bins = Vec::with_capacity(64); // guess
        for path in paths {
            let dir = std::path::PathBuf::from(path);
            if let Ok(entries) = fs::read_dir(&dir) {
                for entry in entries {
                    let entry = entry?;
                    let ft = entry.file_type()?;
                    if ft.is_file() || ft.is_symlink() {
                        bins.push(entry.path());
                    }
                }
            }
        }
        bins.dedup_by_key(|p| p.file_name().unwrap().to_owned());
        for path in bins {
            stdin.write_all(path.file_name().unwrap().as_bytes())?;
            stdin.write_all(DELIM.as_bytes())?;
            stdin.write_all(path.as_os_str().as_bytes())?;
            stdin.write_all(b"\n")?;
        }
    }
    Ok(())
}

pub(super) fn go() -> anyhow::Result<()> {
    unsafe { env::remove_var("ITERM_PROFILE") };

    #[cfg(target_os = "macos")]
    let p = "/Users/langston/.launcher";
    #[cfg(target_os = "linux")]
    let p = "/home/langston/.launcher";

    if let Ok(stdin) = fs::File::open(p) {
        Err(process::Command::new("pick")
            .stdin(stdin)
            .arg(format!("--delimiter={DELIM}"))
            .arg("--with-nth={1}")
            .arg("--bind=enter:become({2})")
            .arg("--preview-window=hidden")
            .exec())?;
    }
    let mut stdin = fs::File::create(p).with_context(|| format!("Failed to create {p}"))?;

    apps(&mut stdin)?;

    stdin.write_all("Tasks".as_bytes())?;
    stdin.write_all(DELIM.as_bytes())?;
    #[cfg(target_os = "macos")]
    stdin.write_all("/Users/langston/code/dots/files/scripts/bin/tasks".as_bytes())?;
    #[cfg(target_os = "linux")]
    stdin.write_all("tasks".as_bytes())?;
    stdin.write_all(b"\n")?;

    stdin.write_all("Password (mpw)".as_bytes())?;
    stdin.write_all(DELIM.as_bytes())?;
    stdin.write_all("mpw".as_bytes())?;
    stdin.write_all(b"\n")?;

    stdin.flush()?;
    stdin.rewind()?;
    let stdin = fs::File::open(p)?;
    Err(process::Command::new("pick")
        .stdin(stdin)
        .arg(format!("--delimiter={DELIM}"))
        .arg("--with-nth={1}")
        .arg("--bind=enter:become(run {2})")
        .arg("--preview-window=hidden")
        .exec())?
}
