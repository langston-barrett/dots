use std::error::Error;

use tracing::{Level, debug};
use tracing_subscriber::fmt::format::FmtSpan;

mod cli;
mod edit;
mod expand;
mod fragments;
mod hook;
mod install;
mod launcher;
mod preview;
mod prompt;
mod system;
mod whitespace;

use cli::{Cli, Command};

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

fn main() -> Result<(), Box<dyn Error>> {
    let cli: Cli = clap::Parser::parse();
    let verbose = verbosity_to_log_level(cli.verbose);
    init_tracing(verbose);
    debug!(?cli);
    match cli.cmd {
        Command::Edit(conf) => edit::go(conf)?,
        Command::Expand(conf) => expand::go(conf)?,
        Command::Fragments(conf) => fragments::go(conf)?,
        Command::Hook(conf) => hook::go(conf)?,
        Command::Launcher => launcher::go()?,
        Command::Install => install::go()?,
        Command::Preview(conf) => preview::go(conf)?,
        Command::Prompt => prompt::go()?,
        Command::Whitespace(conf) => whitespace::go(conf)?,
    }
    Ok(())
}
