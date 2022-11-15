#!/usr/bin/env bash

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export BASH_CONFIG_DIR="${XDG_CONFIG_HOME}/bash"
source_all() { [[ -d $1 ]] && for f in "${1}"/*.sh; do source "${f}"; done; unset f; }
source_all ${XDG_CONFIG_HOME}/sh.d
source_all ${BASH_CONFIG_DIR}/bash.d