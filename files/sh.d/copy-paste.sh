#!/usr/bin/env bash
# ^-- for shellcheck

if [[ ${OSTYPE} == darwin* ]]; then
  copy() {
    if [[ -n "${1}" ]]; then
      pbcopy <"${1}"
    else
      pbcopy
    fi
  }
else
  copy() {
    if [[ -n "${1}" ]]; then
      xsel -ib <"${1}"
    else
      xsel -ib
    fi
  }
fi

if [[ ${OSTYPE} == darwin* ]]; then
  paste() { pbpaste; }
else
  paste() { xsel -ob; }
fi
