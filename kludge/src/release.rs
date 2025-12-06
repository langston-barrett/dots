use std::{error::Error, process::Command};

pub(super) fn go() -> Result<(), Box<dyn Error>> {
    Command::new("git").arg("checkout").arg("main").output()?;
    Ok(())
}
