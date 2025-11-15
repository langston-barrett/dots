#!/usr/bin/env python

"""Run formatters and linters incrementally and in parallel using Ninja"""

# To run on every change:
#
#     git ls-files | entr -c -s './scripts/lint/lint.py'
#
# As a git pre-commit hook:
#
#     bat <<'EOF' > .git/hooks/pre-commit
#     #!/usr/bin/env bash
#     ./scripts/lint/lint.py
#     EOF
#     chmod +x .git/hooks/pre-commit

from argparse import ArgumentParser
from itertools import chain
from os import execvp, environ
from pathlib import Path
from subprocess import run


ninja = r"""
builddir=.out/

# ---------------------------------------------------------
# text

rule bom
  command = rg '\xEF\xBB\xBF' -- $in && exit 1 || touch $out
  description = check for utf-8 byte-order mark

rule crlf
  command = rg --multiline '\r\n' -- $in && exit 1 || touch $out
  description = check for crlf

rule merge
  command = grep -E '^(<<<<<<<|=======|>>>>>>>)' -- $in && exit 1 || touch $out
  description = check for merge conflict markers

rule ws
  command = ./scripts/lint/whitespace.py -- $in && touch $out
  description = whitespace

# ---------------------------------------------------------
# github actions

rule zizmor
  command = zizmor --quiet -- $in && touch $out
  description = zizmor

# ---------------------------------------------------------
# json

rule jq
  command = jq null -- $in > /dev/null && touch $out
  description = jq

# ---------------------------------------------------------
# markdown

rule mdlynx
  command = mdlynx $in && touch $out
  description = mdlynx

rule typos
  command = typos $in && touch $out
  description = typos

# ---------------------------------------------------------
# python (scripts)

rule mypy
  command = mypy --no-error-summary --strict -- $in && touch $out
  description = mypy

rule py
  command = ./scripts/lint/py.py -- $in && touch $out
  description = python style

rule ruff-check
  command = ruff check --quiet -- $in && touch $out
  description = ruff check

rule ruff-fmt
  command = ruff format --quiet -- $in && touch $out
  description = ruff format

rule ruff-fmt-check
  command = ruff format --check --quiet -- $in && touch $out
  description = ruff format --check

# ---------------------------------------------------------
# shell

rule sc
  command = shellcheck --shell=bash -- $in && touch $out
  description = shellcheck

# ---------------------------------------------------------
# rust

rule cargo-clippy
  command = cd kludge; cargo clippy --all-targets --quiet -- --deny warnings && touch ../$out
  description = cargo clippy

rule cargo-fmt
  command = cd kludge; cargo fmt && touch ../$out
  description = cargo fmt

rule cargo-fmt-check
  command = cd kludge; cargo fmt --check && touch ../$out
  description = cargo fmt --check

"""


def build(out: str, rule: str, ins: str, /, *, default: bool = True) -> None:
    global ninja
    assert " " not in out
    ninja += f"build $builddir/{out}: {rule} {ins}\n"
    if default:
        ninja += f"default $builddir/{out}\n"


def lint(rule: str, ins: str, /, *, default: bool = True) -> None:
    slug = ins.replace("/", "-") + "." + rule
    build(slug, rule, ins, default=default)


def ls_files(pat: str) -> list[str]:
    out = run(
        ["git", "ls-files", "--exclude-standard", "--", pat],
        capture_output=True,
        shell=False,
    )
    return out.stdout.strip().decode("utf-8").split("\n")


def txt(path: str) -> None:
    if environ.get("CI") is None:
        # requires rg
        lint("bom", path)
        lint("crlf", path)
    lint("merge", path)
    lint("ws", path)


def gha() -> None:
    gha = ls_files(".github/workflows/*.yml")
    for path in gha:
        lint("zizmor", path)
        txt(path)


def json() -> None:
    json = ls_files("*.json")
    for path in json:
        lint("jq", path)
        txt(path)


def md() -> None:
    md = ls_files("*.md")
    for path in md:
        lint("mdlynx", path)
        lint("typos", path)
        txt(path)


def nix() -> None:
    nix = ls_files("*.nix")
    for path in nix:
        txt(path)


def py(format: bool) -> None:
    py = ls_files("*.py")
    for path in py:
        if Path(path).read_text().startswith("# noqa"):
            continue
        lint("mypy", path)
        lint("ruff-check", path)
        lint("ruff-fmt", path, default=format)
        lint("ruff-fmt-check", path, default=not format)
        lint("py", path)
        txt(path)


def rs(format: bool) -> None:
    rs = ls_files("*.rs")
    build("cargo-clippy", "cargo-clippy", " ".join(rs + ["kludge/Cargo.toml"]))
    build("cargo-fmt", "cargo-fmt", " ".join(rs), default=format)
    build("cargo-fmt-check", "cargo-fmt-check", " ".join(rs), default=not format)
    for path in rs:
        txt(path)


def sh() -> None:
    sh = chain(ls_files("*.sh"), ls_files("files/scripts/bin/*"), ls_files("*.zsh"))
    for path in sh:
        lint("sc", path)
        txt(path)


def ok() -> None:
    if environ.get("CI") is not None:
        return
    rules = [line.split()[1] for line in ninja.splitlines() if line.startswith("rule")]
    for rule in rules:
        ok = False
        for line in ninja.splitlines():
            if line.startswith("build") and f": {rule}" in line:
                ok = True
                break
        assert ok, f"{rule} not in any `build` lines"


def go(format: bool) -> None:
    gha()
    json()
    md()
    nix()
    py(format)
    rs(format)
    sh()
    ok()
    Path("build.ninja").write_text(ninja)
    execvp("ninja", ["ninja"])


parser = ArgumentParser(description=__doc__)
parser.add_argument("--format", action="store_true")
args = parser.parse_args()
go(args.format)
