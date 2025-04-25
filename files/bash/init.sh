#!/usr/bin/env bash

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export BASH_CONFIG_DIR="${XDG_CONFIG_HOME}/bash"
source_all() { [[ -d $1 ]] && for f in "${1}"/*.sh; do source "${f}"; done; unset f; }
source_all ${XDG_CONFIG_HOME}/sh.d
source_all ${BASH_CONFIG_DIR}/bash.d

source_if_exists() { [ -e "${1}" ] && . "${1}"; }
source_if_exists ~/.ghcup/env
source_if_exists ~/.nix-profile/etc/profile.d/nix.sh 
# macOS: https://github.com/NixOS/nix/issues/3616
source_if_exists /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
