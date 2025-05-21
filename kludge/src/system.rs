use std::path::Path;

#[derive(Clone, Copy)]
pub(crate) enum System {
    Cabal,
    Cargo,
    Make,
}

fn any_ancestor<P: AsRef<Path>, T, R: Copy>(
    path: P,
    data: &[(T, R)],
    mut f: impl FnMut(&Path, &T) -> Option<bool>,
) -> Option<R> {
    for a in path.as_ref().ancestors() {
        for (datum, r) in data {
            if f(a, datum)? {
                return Some(*r);
            }
        }
    }
    None
}

fn any_ancestor_contains<P: AsRef<Path>, T, R: Copy>(
    path: P,
    data: &[(T, R)],
    mut f: impl FnMut(&Path, &T) -> Option<bool>,
) -> Option<R> {
    for a in path.as_ref().ancestors() {
        for file in a.read_dir().ok()? {
            let file = file.ok()?;
            let p = file.path();
            for (datum, r) in data {
                if f(&p, datum)? {
                    return Some(*r);
                }
            }
        }
    }
    None
}

fn any_ancestor_contains_file_with_suffix<P: AsRef<Path>, R: Copy>(
    path: P,
    suffixes: &[(&str, R)],
) -> Option<R> {
    any_ancestor_contains(path, suffixes, |p, suf| Some(p.ends_with(suf)))
}

fn any_ancestor_has<P: AsRef<Path>, R: Copy>(path: P, suffixes: &[(&str, R)]) -> Option<R> {
    any_ancestor(path, suffixes, |p, suf| Some(p.join(suf).exists()))
}

impl System {
    pub(crate) fn detect<P: AsRef<Path>>(path: P) -> Option<Self> {
        let path: &Path = path.as_ref();
        let files = &[
            ("cabal.project", Self::Cabal),
            ("Cargo.toml", Self::Cargo),
            ("Makefile", Self::Make),
        ];
        if let Some(b) = any_ancestor_has(path, files) {
            return Some(b);
        }
        let suffixes = &[(".cabal", Self::Cabal), (".makefile", Self::Make)];
        if let Some(b) = any_ancestor_contains_file_with_suffix(path, suffixes) {
            return Some(b);
        }
        None
    }
}
