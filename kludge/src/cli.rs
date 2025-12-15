#[derive(Debug, clap::Parser)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) cmd: Command,

    /// Verbose mode: use multiple times for increased verbosity
    #[arg(
        long,
        short = 'v',
        action = clap::ArgAction::Count,
    )]
    pub(crate) verbose: u8,
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum Command {
    Edit(crate::edit::Config),
    Fragments(crate::fragments::Config),
    Install,
    Launcher,
    Precommit,
    Preview(crate::preview::Config),
    Project,
    Release(crate::release::Config),
    Zsh(Zsh),
}

#[derive(Debug, clap::Parser)]
pub(crate) struct Zsh {
    #[command(subcommand)]
    pub(crate) cmd: ZshCommand,
}

/// Functionality used by zsh
#[derive(Debug, clap::Subcommand)]
pub(crate) enum ZshCommand {
    Expand(crate::expand::Config),
    Hook(crate::hook::Config),
    /// Print information for ZSH prompt
    Prompt,
}
