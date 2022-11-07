#!/usr/bin/env bash

set -e
shopt -s globstar

for f in ./**/*.zsh; do
  if ! [[ $f =~ "prompt.zsh" ]]; then
    echo "linting $f"
    shellcheck \
      --shell=bash \
      --exclude=SC1071 \
      --exclude=SC1090 \
      "${f}"
  fi
done
