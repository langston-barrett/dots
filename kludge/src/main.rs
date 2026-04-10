#![warn(clippy::expect_used)]
#![warn(clippy::panic)]
#![warn(clippy::unwrap_used)]
#![cfg_attr(test, allow(clippy::expect_used))]
#![cfg_attr(test, allow(clippy::panic))]
#![cfg_attr(test, allow(clippy::unwrap_used))]

use clap::Parser as _;
use tracing::{Level, trace};
use tracing_subscriber::fmt::format::FmtSpan;

mod cli;
mod edit;
mod expand;
mod format;
mod fragments;
mod hook;
mod install;
mod launcher;
mod lint;
mod postcommit;
mod precommit;
mod preview;
mod project;
mod prompt;
mod release;
mod system;

use cli::Command;

use crate::cli::ZshCommand;

fn verbosity_to_log_level(verbosity: u8) -> Level {
    match verbosity {
        0 => Level::WARN,
        1 => Level::INFO,
        2 => Level::DEBUG,
        _ => Level::TRACE,
    }
}

fn init_tracing(level: Level) {
    let builder = tracing_subscriber::fmt::fmt()
        .with_target(false)
        .with_max_level(level)
        .with_writer(std::io::stderr);
    if let Level::TRACE = level {
        let builder = builder.with_span_events(FmtSpan::ENTER | FmtSpan::CLOSE);
        builder.init();
    } else {
        let builder = builder.without_time();
        builder.init();
    }
}

fn main() -> anyhow::Result<()> {
    let cli = cli::Cli::parse();
    let verbose = verbosity_to_log_level(cli.verbose);
    init_tracing(verbose);
    trace!(?cli);
    go(cli)?;
    Ok(())
}

fn go(cli: cli::Cli) -> anyhow::Result<()> {
    match cli.cmd {
        Command::Edit(conf) => edit::go(conf),
        Command::Format(conf) => format::go(conf),
        Command::Fragments(conf) => fragments::go(conf),
        Command::Launcher => launcher::go(),
        Command::Install => install::go(),
        Command::Lint(conf) => lint::go(conf),
        Command::Postcommit => postcommit::go(),
        Command::Precommit => precommit::go(),
        Command::Preview(conf) => preview::go(conf),
        Command::Project => project::go(),
        Command::Release(conf) => release::go(conf),
        Command::Zsh(zsh) => match zsh.cmd {
            ZshCommand::Expand(conf) => expand::go(conf),
            ZshCommand::Hook(conf) => hook::go(conf),
            ZshCommand::Prompt => prompt::go(),
        },
    }
}
