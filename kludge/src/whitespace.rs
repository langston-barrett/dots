use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    process,
};

/// Detect and fix whitespace issues
#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[clap(long)]
    check: bool,
    #[clap(long, default_value = "4")]
    tab_width: usize,
    paths: Vec<PathBuf>,
}

fn fix_whitespace(path: &Path, check: bool, tab_width: usize) -> Result<bool, Box<dyn Error>> {
    let path_bytes = path.as_os_str().as_encoded_bytes();
    let tabs_ok =
        path_bytes == "Makefile".as_bytes() || path_bytes.ends_with(".makefile".as_bytes());
    let tab = " ".repeat(tab_width);
    let bytes = fs::read(path)?;
    if let Ok(content) = std::str::from_utf8(bytes.as_slice()) {
        let mut lines = Vec::with_capacity(64); // guess
        let mut ok = true;
        for (num, line) in content.lines().enumerate() {
            let num = num + 1; // line numbers start at 1
            let trimmed = line.trim_end();
            if check {
                if line != trimmed {
                    ok = false;
                    let col = trimmed.len();
                    eprintln!("{}:{}:{}: trailing whitespace", path.display(), num, col);
                }
                if !tabs_ok && let Some(idx) = line.find('\t') {
                    ok = false;
                    let col = idx + 1; // column numbers start at 1
                    eprintln!("{}:{}:{}: tab", path.display(), num, col);
                }
            } else {
                lines.push(if tabs_ok {
                    trimmed.to_owned()
                } else {
                    trimmed.replace('\t', &tab)
                });
            }
        }
        if check {
            return Ok(ok);
        }
        let fixed = lines.join("\n") + "\n";
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
            ok &= fix_whitespace(&path, conf.check, conf.tab_width)?;
            continue;
        }
        if path.is_dir() {
            for entry in fs::read_dir(&path)? {
                let entry = entry?;
                stack.push(entry.path().clone());
            }
        }
    }

    if conf.check && !ok {
        process::exit(1);
    }

    Ok(())
}
