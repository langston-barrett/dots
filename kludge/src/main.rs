mod build;
mod cli;
mod zle;

use cli::{Cli, Commands};

fn main() {
    let cli: Cli = clap::Parser::parse();

    match cli.cmd {
        Commands::Zle(conf) => zle::go(conf),
    }
}
