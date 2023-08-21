use tracing::{debug, Level};
use tracing_subscriber::fmt::format::FmtSpan;

mod build;
mod cli;
mod zle;

use cli::{Cli, Commands};

fn verbosity_to_log_level(verbosity: u8) -> Level {
    match verbosity {
        0 => Level::WARN,
        1 => Level::INFO,
        2 => Level::DEBUG,
        _ => Level::TRACE,
    }
}

fn initialize_tracing(cli: &Cli) {
    tracing_subscriber::fmt::fmt()
        .with_span_events(FmtSpan::NONE)
        .with_target(false)
        .with_max_level(verbosity_to_log_level(cli.verbose))
        .init();
}

fn main() {
    let cli: Cli = clap::Parser::parse();

    initialize_tracing(&cli);
    debug!(?cli);

    match cli.cmd {
        Commands::Zle(conf) => zle::go(conf),
    }
}
