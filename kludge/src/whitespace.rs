use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    process,
};

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[clap(long)]
    check: bool,
    paths: Vec<PathBuf>,
}

fn fix_whitespace(path: &Path, check: bool) -> Result<bool, Box<dyn Error>> {
    let bytes = fs::read(path)?;
    if let Ok(content) = std::str::from_utf8(bytes.as_slice()) {
        let fixed = content
            .lines()
            .map(|line| line.trim_end())
            .collect::<Vec<&str>>()
            .join("\n")
            + "\n";
        if check {
            return Ok(content == fixed);
        }
        fs::write(path, fixed)?;
    }
    Ok(true)
}

pub(super) fn go(mut conf: Config) -> Result<(), Box<dyn Error>> {
    let mut stack = Vec::with_capacity(conf.paths.len());
    conf.paths.dedup();
    stack.extend(conf.paths.into_iter());

    let mut ok = true;
    while let Some(path) = stack.pop() {
        if path.is_file() {
            ok |= fix_whitespace(&path, conf.check)?;
            continue;
        }
        if path.is_dir() {
            for entry in fs::read_dir(&path)? {
                let entry = entry?;
                stack.push(entry.path().to_owned());
            }
        }
    }

    if conf.check && !ok {
        process::exit(1);
    }

    Ok(())
}
