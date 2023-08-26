use std::env;
use std::error::Error;
use std::process::Command;

// NB: Spacing is a Unicode em-space
const PROMPT_SEP: &str = " : ";

fn git_branch_name() -> Option<String> {
    let out = Command::new("git")
        .arg("symbolic-ref")
        .arg("--short")
        .arg("HEAD")
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let nm = String::from_utf8_lossy(out.stdout.as_slice());
    Some(nm.trim_end().to_string())
}

pub(super) fn go() -> Result<(), Box<dyn Error>> {
    for dir in env::current_dir()?.ancestors() {
        if dir.join(".git").is_dir() {
            if let Some(nm) = git_branch_name() {
                println!("{PROMPT_SEP}{nm}");
            }
            break;
        }
    }
    Ok(())
}
