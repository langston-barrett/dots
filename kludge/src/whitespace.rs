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
    #[clap(long, default_value = "4")]
    tab_width: usize,
    paths: Vec<PathBuf>,
}

fn fix_line(line: &str, tab_width: usize) -> String {
    line.replace("\t", &" ".repeat(tab_width))
        .trim_end()
        .to_string()
}

fn fix_whitespace(path: &Path, check: bool, tab_width: usize) -> Result<bool, Box<dyn Error>> {
    let bytes = fs::read(path)?;
    if let Ok(content) = std::str::from_utf8(bytes.as_slice()) {
        let fixed = content
            .lines()
            .map(|line| fix_line(line, tab_width))
            .collect::<Vec<_>>()
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
    stack.extend(conf.paths);

    let mut ok = true;
    while let Some(path) = stack.pop() {
        if path.is_file() {
            ok |= fix_whitespace(&path, conf.check, conf.tab_width)?;
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
