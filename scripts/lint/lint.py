#!/usr/bin/env python

"""Run formatters and linters incrementally and in parallel using Ninja"""

# TODO: rumdl
# TODO: sh -n
# TODO: taplo

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
from enum import IntFlag
from os import execvp, environ
from pathlib import Path
from subprocess import run
from textwrap import dedent
from typing import NewType, cast

NinjaScript = NewType("NinjaScript", str)


class Mode(IntFlag):
    lint = 1
    format = 2
    fix = 4


def build(
    ninja: NinjaScript, out: str, rule: str, ins: str, /, *, default: bool = True
) -> NinjaScript:
    assert " " not in out
    ninja = cast(NinjaScript, ninja + f"build $builddir/{out}: {rule} {ins}\n")
    if default:
        ninja = cast(NinjaScript, ninja + f"default $builddir/{out}\n")
    return ninja


def rules(ninja: NinjaScript, rule_def: str) -> NinjaScript:
    return cast(NinjaScript, ninja + dedent(rule_def))


def lint(
    ninja: NinjaScript, rule: str, ins: str, /, *, default: bool = True
) -> NinjaScript:
    # replace directory separators `/` with hyphens `-`
    slug = ins.replace("/", "-") + "." + rule
    return build(ninja, slug, rule, ins, default=default)


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


def txt(ninja: NinjaScript, path: str, mode: Mode) -> NinjaScript:
    if environ.get("CI") is None:
        # requires rg
        ninja = lint(ninja, "bom", path)
        ninja = lint(ninja, "crlf", path)
    ninja = lint(ninja, "merge", path)
    ninja = lint(ninja, "ws", path, default=bool(mode & Mode.lint))
    ninja = lint(ninja, "ws-fix", path, default=bool(mode & Mode.format))
    return ninja


def gha(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    gha = ls_files([".github/**/*.yml"])
    if gha == []:
        return ninja

    ninja = rules(
        ninja,
        """
    rule zizmor
      command = zizmor --quiet -- $in && touch $out
      description = zizmor
    """,
    )
    for path in gha:
        if path.endswith("workflows/dependabot.yml"):
            # https://github.com/zizmorcore/zizmor/issues/1341
            continue
        ninja = lint(ninja, "zizmor", path)
        ninja = txt(ninja, path, mode)
    return ninja


def json(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    json = ls_files(["*.json"])
    if json == []:
        return ninja

    ninja = rules(
        ninja,
        """
    rule jq
      command = jq null -- $in > /dev/null && touch $out
      description = jq
    """,
    )
    for path in json:
        ninja = lint(ninja, "jq", path)
        ninja = txt(ninja, path, mode)
    return ninja


def make(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    make = ls_files(["**/Makefile"])
    if make == []:
        return ninja

    ninja = rules(
        ninja,
        """
    rule make-n
      command = make -n -f $$in && touch $out
      description = make -n
    """,
    )
    for path in make:
        ninja = lint(ninja, "make-n", path)
        ninja = txt(ninja, path, mode)
    return ninja


def md(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    md = ls_files(["*.md"])
    if md == []:
        return ninja

    ninja = rules(
        ninja,
        """
    rule mdlynx
      command = mdlynx $in && touch $out
      description = mdlynx

    rule typos
      command = typos $in && touch $out
      description = typos
    """,
    )
    for path in md:
        ninja = lint(ninja, "mdlynx", path)
        ninja = lint(ninja, "typos", path)
        ninja = txt(ninja, path, mode)
    return ninja


def nix(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    nix = ls_files(["*.nix"])
    if nix == []:
        return ninja

    for path in nix:
        ninja = txt(ninja, path, mode)
    return ninja


def py(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    py = ls_files(["*.py"])
    if py == []:
        return ninja

    ninja = rules(
        ninja,
        """
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
    """,
    )
    for path in py:
        if Path(path).read_text().startswith("# noqa"):
            continue
        ninja = lint(ninja, "mypy", path)
        ninja = lint(ninja, "ruff-check", path)
        ninja = lint(ninja, "ruff-fmt", path, default=bool(mode & Mode.format))
        ninja = lint(ninja, "ruff-fmt-check", path, default=bool(mode & Mode.lint))
        ninja = lint(ninja, "py", path)
        ninja = txt(ninja, path, mode)
    return ninja


def rs(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    cargo = ls_files(["**/Cargo.toml"])
    rs = ls_files(["*.rs"])
    if rs == []:
        return ninja

    ninja = rules(
        ninja,
        """
    rule cargo-clippy
      command = cd kludge; cargo clippy --all-targets --quiet -- --deny warnings && touch ../$out
      description = cargo clippy

    rule cargo-fmt
      command = cd kludge; cargo fmt && touch ../$out
      description = cargo fmt

    rule cargo-fmt-check
      command = cd kludge; cargo fmt --check && touch ../$out
      description = cargo fmt --check
    """,
    )
    ninja = build(ninja, "cargo-clippy", "cargo-clippy", " ".join(cargo + rs))
    ninja = build(
        ninja, "cargo-fmt", "cargo-fmt", " ".join(rs), default=bool(mode & Mode.format)
    )
    ninja = build(
        ninja,
        "cargo-fmt-check",
        "cargo-fmt-check",
        " ".join(rs),
        default=bool(mode & Mode.lint),
    )
    for path in rs:
        ninja = txt(ninja, path, mode)
    return ninja


def sh(ninja: NinjaScript, mode: Mode) -> NinjaScript:
    sh = ls_files(["*.sh", "files/scripts/bin/*", "*.zsh"])
    if sh == []:
        return ninja

    ninja = rules(
        ninja,
        """
    rule sc
      command = shellcheck --shell=bash -- $in && touch $out
      description = shellcheck
    """,
    )
    for path in sh:
        ninja = lint(ninja, "sc", path)
        ninja = txt(ninja, path, mode)
    return ninja


def xref(ninja: NinjaScript) -> NinjaScript:
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
        return ninja

    ninja = rules(
        ninja,
        """
    rule xref
      command = ./scripts/lint/xref.py -- $in && touch $out
      description = xref
    """,
    )
    ninja = build(ninja, "xref", "xref", " ".join(files))
    return ninja


def ok(ninja: NinjaScript) -> NinjaScript:
    if environ.get("CI") is not None:
        return ninja
    rules = [line.split()[1] for line in ninja.splitlines() if line.startswith("rule")]
    for rule in rules:
        ok = False
        for line in ninja.splitlines():
            if line.startswith("build") and f": {rule}" in line:
                ok = True
                break
        assert ok, f"{rule} not in any `build` lines"
    return ninja


def go(mode: Mode) -> None:
    ninja = NinjaScript(
        dedent(r"""
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

    rule ws-fix
      command = ./scripts/lint/whitespace.py --fix -- $in && touch $out
      description = whitespace --fix
    """)
    )
    ninja = gha(ninja, mode)
    ninja = json(ninja, mode)
    ninja = md(ninja, mode)
    ninja = make(ninja, mode)
    ninja = nix(ninja, mode)
    ninja = py(ninja, mode)
    ninja = rs(ninja, mode)
    ninja = sh(ninja, mode)
    ninja = xref(ninja)
    ninja = ok(ninja)
    Path("build.ninja").write_text(ninja)
    execvp("ninja", ["ninja"])


parser = ArgumentParser(description=__doc__)
parser.add_argument("--format", action="store_true")
parser.add_argument("--fix", action="store_true")
args = parser.parse_args()
mode = Mode(0)
if not (args.format or args.fix):
    mode |= Mode.lint
if args.format:
    mode |= Mode.format
if args.fix:
    mode |= Mode.fix
go(mode)
