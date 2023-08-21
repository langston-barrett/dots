use std::path::Path;

pub(crate) enum System {
    Cabal,
    Cargo,
    Make,
}

impl System {
    pub(crate) fn detect<P: AsRef<Path>>(path: P) -> Option<Self> {
        let path: &Path = path.as_ref();
        if path.join("cabal.project").exists() {
            return Some(Self::Cabal);
        }
        if path.join("Cargo.toml").exists() {
            return Some(Self::Cargo);
        }
        if path.join("Makefile").exists() {
            return Some(Self::Make);
        }
        for file in path.read_dir().ok()? {
            let file = file.ok()?;
            let path = file.path();
            if path.ends_with(".cabal") {
                return Some(Self::Cabal);
            }
            if path.ends_with(".makefile") {
                return Some(Self::Make);
            }
        }
        None
    }
}
