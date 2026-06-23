#!/usr/bin/env bash

eval "$(zbr init "${HOME}"/code/zbr/conf/conf.toml)"

if command -v kludge >/dev/null 2>&1; then
  eval "$(kludge zsh init)"
fi
