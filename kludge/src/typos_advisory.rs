use std::{
    collections::BTreeSet,
    io::Write as _,
    process::{Command, Stdio},
};

fn comment_prefix(ext: &str) -> Option<&'static str> {
    match ext {
        "hs" => Some("--"),
        "py" | "sh" | "nix" => Some("#"),
        "rs" => Some("//"),
        _ => None,
    }
}

fn get_diff(staged: bool) -> Option<String> {
    let mut cmd = Command::new("git");
    if staged {
        cmd.args(["diff", "--cached", "-U0"]);
    } else {
        cmd.args(["diff", "-U0", "HEAD~1", "HEAD"]);
    }
    let out = cmd.output().ok()?;
    if !out.status.success() {
        return None;
    }
    String::from_utf8(out.stdout).ok()
}

struct FileInfo {
    path: String,
    prefix: &'static str,
    added_comment_lines: BTreeSet<usize>,
}

fn parse_diff(diff: &str) -> Vec<FileInfo> {
    let mut files: Vec<FileInfo> = Vec::new();
    let mut current: Option<usize> = None;
    let mut new_line: usize = 0;

    for line in diff.lines() {
        if let Some(rest) = line.strip_prefix("+++ b/") {
            let ext = rest.rsplit('.').next().unwrap_or("");
            if let Some(prefix) = comment_prefix(ext) {
                files.push(FileInfo {
                    path: rest.to_owned(),
                    prefix,
                    added_comment_lines: BTreeSet::new(),
                });
                current = Some(files.len() - 1);
            } else {
                current = None;
            }
            continue;
        }

        // @@ -old_start[,count] +new_start[,count] @@
        if let Some(rest) = line.strip_prefix("@@ ") {
            if let Some(plus) = rest.find('+') {
                let s = &rest[plus + 1..];
                let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
                new_line = s[..end].parse().unwrap_or(1);
            }
            continue;
        }

        let Some(idx) = current else { continue };

        if let Some(content) = line.strip_prefix('+') {
            let trimmed = content.trim_start();
            if trimmed.starts_with(files[idx].prefix) {
                files[idx].added_comment_lines.insert(new_line);
            }
            new_line += 1;
        } else if line.starts_with(' ') {
            new_line += 1;
        }
    }

    files.retain(|f| !f.added_comment_lines.is_empty());
    files
}

fn staged_content(path: &str) -> Option<String> {
    let out = Command::new("git")
        .args(["show", &format!(":{path}")])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    String::from_utf8(out.stdout).ok()
}

fn sparse_content(content: &str, keep_lines: &BTreeSet<usize>) -> String {
    let mut out = String::with_capacity(content.len());
    for (i, line) in content.lines().enumerate() {
        if keep_lines.contains(&(i + 1)) {
            out.push_str(line);
        }
        out.push('\n');
    }
    out
}

fn check_file(info: &FileInfo, staged: bool) {
    let content = if staged {
        staged_content(&info.path)
    } else {
        std::fs::read_to_string(&info.path).ok()
    };
    let Some(content) = content else { return };

    let sparse = sparse_content(&content, &info.added_comment_lines);

    let Ok(mut child) = Command::new("typos")
        .arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    else {
        return;
    };

    if let Some(mut stdin) = child.stdin.take() {
        drop(stdin.write_all(sparse.as_bytes()));
    }

    if let Ok(out) = child.wait_with_output() {
        let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
        text.push_str(&String::from_utf8_lossy(&out.stderr));
        let text = text.replace("<stdin>", &info.path);
        if !text.is_empty() {
            eprint!("{text}");
        }
    }
}

pub(crate) fn run(staged: bool) {
    let Some(diff) = get_diff(staged) else { return };
    let files = parse_diff(&diff);
    for info in &files {
        check_file(info, staged);
    }
}
