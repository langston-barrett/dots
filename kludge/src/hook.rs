use std::collections::HashSet;

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
    let words = conf
        .cmd
        .iter()
        .map(String::as_str)
        .collect::<HashSet<&str>>();
    for word in IGNORE.iter().copied() {
        if words.contains(word) {
            return;
        }
    }
    // TODO: figure out false positives
    if duration == usize::MAX {
        notify(format!("{} finished after {duration}s", conf.cmd.join(" ")));
    }
}

#[allow(clippy::unnecessary_wraps)]
pub(super) fn go(conf: Config) -> anyhow::Result<()> {
    match conf.hook {
        Hook::Begin(conf) => begin(conf),
        Hook::End(conf) => end(conf),
    }
    Ok(())
}
