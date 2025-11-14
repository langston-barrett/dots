#!/usr/bin/env bash

set -e

function report() {
  printf "%s" "$1" > "/tmp/$(basename "${PWD}")-status"
}

function format() {
  report "f: ${1} | b: ${2} | l: ${3}"
}

function rust() {
  local f="?"
  local b="?"
  local l="?"
  format "${f}" "${b}" "${l}"
  if cargo fmt --check; then
    f="✓"
  else
    f="❌"
  fi
  format "${f}" "${b}" "${l}"
  if cargo check; then
    b="✓"
  else
    b="❌"
    l="❌"
    format "${f}" "${b}" "${l}"
    return
  fi
  if cargo clippy -- --deny warnings; then
    l="✓"
  else
    l="❌"
  fi
  format "${f}" "${b}" "${l}"
}

if [[ -f Cargo.toml ]]; then
  rust
fi
