#!/usr/bin/env bash
# ^ For Shellcheck

installed() { command -v "$1" >/dev/null 2>&1; }
if installed tmux && [[ -z "${TMUX}" ]] && [[ -z "${HYDRA}" ]]; then
  tmux
fi
