#[derive(Debug, clap::Parser)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) cmd: Commands,

    /// Verbose mode: use multiple times for increased verbosity
    #[arg(
        long,
        short = 'v',
        action = clap::ArgAction::Count,
    )]
    pub(crate) verbose: u8,
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum Commands {
    Zle(crate::zle::Config),
}
