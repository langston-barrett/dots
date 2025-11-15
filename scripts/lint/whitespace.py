#!/usr/bin/env python3

"""Check for trailing whitespace"""

from argparse import ArgumentParser
from pathlib import Path
from sys import exit


def die(msg: str) -> None:
    print(msg)
    exit(1)


def check(paths: list[Path]) -> None:
    for path in paths:
        if path.is_dir():
            files = list(path.rglob("*"))
        else:
            files = [path]
        for file in files:
            if not file.is_file():
                continue
            try:
                t = file.read_text()
            except UnicodeDecodeError:
                continue
            for no, line in enumerate(t.splitlines()):
                if line != line.rstrip():
                    die(f"{file}:{no + 1}: Trailing whitespace")


parser = ArgumentParser(description=__doc__)
parser.add_argument("paths", nargs="+", type=Path)
args = parser.parse_args()
check(args.paths)
