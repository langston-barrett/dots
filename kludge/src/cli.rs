#[derive(clap::Parser)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) cmd: Commands,
}

#[derive(clap::Subcommand)]
pub(crate) enum Commands {
    Zle(crate::zle::Config),
}
