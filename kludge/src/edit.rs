use std::{
    error::Error,
    fs,
    os::unix::process::CommandExt as _,
    path::{Path, PathBuf},
    process,
};

/// Detect and fix whitespace issues
#[derive(Debug, clap::Parser)]
pub(crate) struct Config {
    paths: Vec<PathBuf>,
}

const C: &str = r#"#include <stdio.h>

int main(int argc, char* argv[]) {
    return 0;
}
"#;

const HS: &str = r#"{-# LANGUAGE ImportQualifiedPost #-}


-- |
-- Description: TODO
-- Copyright: (c) Galois, Inc. 2025
-- License: BSD-3-Clause
-- Maintainer: GREASE Maintainers <grease@galois.com>
--
-- TODO
module Main (
  main,
) where

main :: IO ()
main = pure ()
"#;

const PY: &str = r#"#!/usr/bin/env python3

from argparse import ArgumentParser
from pathlib import Path
from sys import exit, stderr


def eprint(*args, **kwargs):
    print(*args, file=stderr, **kwargs)


def die(msg, /):
    eprint(msg)
    exit(1)


def go(path: Path, /, *, dry_run: bool = False):
    die("Not yet implemented")


def main():
    parser = ArgumentParser()
    parser.add_argument("path", type=Path)
    parser.add_argument("-n", "--dry-run", action="store_true")
    args = parser.parse_args()
    go(args.path, dry_run=args.dry_run)


if __name__ == "__main__":
    main()
"#;

const SH: &str = r#"#!/usr/bin/env bash

set -euo pipefail

log() { printf "%s\n" "${1}" >&2; }
die() { log "${1}"; exit 1; }

die "Not yet implemented"
"#;

fn make_default(path: &Path) -> Result<(), Box<dyn Error>> {
    if !path.exists() {
        match path.extension() {
            Some(s) if s == "c" => fs::write(path, C.as_bytes()),
            Some(s) if s == "hs" => fs::write(path, HS.as_bytes()),
            Some(s) if s == "py" => fs::write(path, PY.as_bytes()),
            Some(s) if s == "sh" => fs::write(path, SH.as_bytes()),
            _ => Ok(()),
        }?;
    }
    Err(process::Command::new("hx").arg(path).exec())?;
    Ok(())
}

pub(super) fn go(conf: Config) -> Result<(), Box<dyn Error>> {
    for path in conf.paths {
        make_default(path.as_path())?;
    }
    Ok(())
}
