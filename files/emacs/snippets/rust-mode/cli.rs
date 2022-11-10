use std::path::PathBuf;

use clap::Parser;

/// My app
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// Temporary directory
    #[arg(short, long, default_value_os_t = PathBuf::from("./temp"))]
    pub dir: PathBuf,

    /// Number of threads
    #[arg(short, long, default_value_t = 4)]
    pub jobs: usize,
}