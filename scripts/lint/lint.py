#!/usr/bin/env python

"""Run formatters and linters incrementally and in parallel using Ninja"""

# TODO: markdownlint
# TODO: make -n
# TODO: sh -n
# TODO: taplo
# TODO: whitespace --fix

# Ninja is essentially a simpler, faster version of Make. A Ninja configuration
# consists of *rules* (`rule`) and some number of *build statements* (`build`).
# A rule is an abbreviation for a shell command, and a build statement is a
# recipe for producing some number of output files (*targets*) from some number
# of input files by running a rule.
#
# Conceptually, we can consider the Ninja configuration as a hypergraph where
# the nodes are files and the hyperedges are build statements, labeled by rules.
# The inputs to Ninja are this hypergraph and a set of desired targets. Ninja
# traverses the hypergraph and recursively runs rules to build everything until
# it can build the desired targets.
#
# Just like Make, Ninja checks *file modification times* to see if rebuilding
# is necessary. If the output was modified more recently than all of the inputs
# (according to filesystem metadata), then Ninja will skip rebuilding that
# target.
#
# See ninja.build for more information about Ninja.
#
# This script generates Ninja configurations to run linters. It works by
# generating build statements that produce one target file (in `.out/`) for each
# combination of linter and input files. For example, for a file `foo/bar.py`
# and the linter `ruff`, it would generate a `build` statement like
#
#     build .out/foo/bar.py: ruff-check foo/bar.py
#
# where the `ruff-check` rule is something like:
#
#     rule ruff-check
#       command = ruff check -- $in && touch $out
#
# Together, these say "to produce output file `.out/foo-bar.py`, run `ruff
# check` on `foo/bar.py` and then (if it succeeds) `touch foo-bar.py`". Hence,
# the rules produce empty output files in `.out/` indicating that the linter has
# been run.
#
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
from os import execvp, environ
from pathlib import Path
from subprocess import run
from textwrap import dedent


ninja = r"""
builddir=.out/

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

"""


def build(out: str, rule: str, ins: str, /, *, default: bool = True) -> None:
    global ninja
    assert " " not in out
    ninja += f"build $builddir/{out}: {rule} {ins}\n"
    if default:
        ninja += f"default $builddir/{out}\n"


def lint(rule: str, ins: str, /, *, default: bool = True) -> None:
    # replace directory separators `/` with hyphens `-`
    slug = ins.replace("/", "-") + "." + rule
    build(slug, rule, ins, default=default)


def ls_files(pats: list[str]) -> list[str]:
    out = run(
        ["git", "ls-files", "--exclude-standard", "--"] + pats,
        capture_output=True,
        shell=False,
    )
    stdout = out.stdout.strip()
    if stdout == b"":
        return []
    return stdout.decode("utf-8").split("\n")


def txt(path: str) -> None:
    if environ.get("CI") is None:
        # requires rg
        lint("bom", path)
        lint("crlf", path)
    lint("merge", path)
    lint("ws", path)


def gha() -> None:
    gha = ls_files([".github/**/*.yml"])
    if gha == []:
        return

    global ninja
    ninja += dedent("""
    rule zizmor
      command = zizmor --quiet -- $in && touch $out
      description = zizmor
    """)
    for path in gha:
        if path.endswith("workflows/dependabot.yml"):
            # https://github.com/zizmorcore/zizmor/issues/1341
            continue
        lint("zizmor", path)
        txt(path)


def json() -> None:
    json = ls_files(["*.json"])
    if json == []:
        return

    global ninja
    ninja += dedent("""
    rule jq
      command = jq null -- $in > /dev/null && touch $out
      description = jq
    """)
    for path in json:
        lint("jq", path)
        txt(path)


def md() -> None:
    md = ls_files(["*.md"])
    if md == []:
        return

    global ninja
    ninja += dedent("""
    rule mdlynx
      command = mdlynx $in && touch $out
      description = mdlynx

    rule typos
      command = typos $in && touch $out
      description = typos
    """)
    for path in md:
        lint("mdlynx", path)
        lint("typos", path)
        txt(path)


def nix() -> None:
    nix = ls_files(["*.nix"])
    if nix == []:
        return

    for path in nix:
        txt(path)


def py(format: bool) -> None:
    py = ls_files(["*.py"])
    if py == []:
        return

    global ninja
    ninja += dedent("""
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
    """)
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
    rs = ls_files(["*.rs"])
    if rs == []:
        return

    global ninja
    ninja += dedent("""
    rule cargo-clippy
      command = cd kludge; cargo clippy --all-targets --quiet -- --deny warnings && touch ../$out
      description = cargo clippy

    rule cargo-fmt
      command = cd kludge; cargo fmt && touch ../$out
      description = cargo fmt

    rule cargo-fmt-check
      command = cd kludge; cargo fmt --check && touch ../$out
      description = cargo fmt --check
    """)
    build("cargo-clippy", "cargo-clippy", " ".join(rs + ["kludge/Cargo.toml"]))
    build("cargo-fmt", "cargo-fmt", " ".join(rs), default=format)
    build("cargo-fmt-check", "cargo-fmt-check", " ".join(rs), default=not format)
    for path in rs:
        txt(path)


def sh() -> None:
    sh = ls_files(["*.sh", "files/scripts/bin/*", "*.zsh"])
    if sh == []:
        return

    global ninja
    ninja += dedent("""
    rule sc
      command = shellcheck --shell=bash -- $in && touch $out
      description = shellcheck
    """)
    for path in sh:
        lint("sc", path)
        txt(path)


def xref() -> None:
    files = ls_files(
        [
            "**/Makefile",
            "*.cabal",
            "*.md",
            "*.mk",
            "*.project",
            "*.py",
            "*.rs",
            "*.scala",
            "*.sh",
            "*.toml",
            "*.zsh",
        ]
    )
    if files == []:
        return

    global ninja
    ninja += dedent("""
    rule xref
      command = ./scripts/lint/xref.py -- $in && touch $out
      description = xref
    """)
    build("xref", "xref", " ".join(files))


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
    xref()
    ok()
    Path("build.ninja").write_text(ninja)
    execvp("ninja", ["ninja"])


parser = ArgumentParser(description=__doc__)
parser.add_argument("--format", action="store_true")
args = parser.parse_args()
go(args.format)
