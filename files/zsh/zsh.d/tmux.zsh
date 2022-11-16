#!/usr/bin/env bash
# ^ For Shellcheck

if which tmux > /dev/null 2>&1 && [[ -z "${TMUX}" ]]; then
  tmux
fi
