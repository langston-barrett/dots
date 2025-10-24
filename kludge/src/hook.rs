use std::{collections::HashSet, error::Error};

#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    #[command(subcommand)]
    pub(crate) hook: Hook,
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum Hook {
    Begin(BeginConfig),
    End(EndConfig),
}

#[derive(Debug, clap::Parser)]
pub(crate) struct BeginConfig {
    #[clap(long)]
    time: usize,
    cmd: Vec<String>,
}

#[derive(Debug, clap::Parser)]
pub(crate) struct EndConfig {
    #[clap(long)]
    begin: usize,
    #[clap(long)]
    end: usize,
    cmd: Vec<String>,
}

const IGNORE: &[&str] = &["ls", "hx"];

fn begin(_conf: BeginConfig) {}

fn notify(s: String) {
    drop(std::process::Command::new("notify").arg(s).spawn());
}

fn end(conf: EndConfig) {
    let duration = conf.end.saturating_sub(conf.begin);
    let words = HashSet::<&str>::from_iter(conf.cmd.iter().map(|s| s.as_str()));
    for word in IGNORE.iter().copied() {
        if words.contains(word) {
            return;
        }
    }
    // TODO: figure out false positives
    if duration >= usize::MAX {
        notify(format!("{} finished after {duration}s", conf.cmd.join(" ")));
    }
}

#[allow(clippy::unnecessary_wraps)]
pub(super) fn go(conf: Config) -> Result<(), Box<dyn Error>> {
    match conf.hook {
        Hook::Begin(conf) => begin(conf),
        Hook::End(conf) => end(conf),
    }
    Ok(())
}
