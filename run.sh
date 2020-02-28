#!/usr/bin/env bash

set -e

installed() { command -v "$1" >/dev/null 2>&1; }
bail_if_not_installed() {
  if ! installed "$1"; then
    echo "$0 requires $1, but it isn't on the PATH."
    exit 1
  fi
}

bail_if_not_installed curl
bail_if_not_installed tar

if ! [[ -f scripts/run-ansible.sh ]]; then
  dir=$(mktemp -d)
  pushd "$dir"
  curl \
    --location \
    --output "$PWD/dots.tar.gz" \
    "https://github.com/langston-barrett/dots/tarball/master"
  tar xvf dots.tar.gz
  pushd ./langston-barrett-dots*/
  bash run.sh
  popd
  popd

else
  if ! installed nix; then
    curl https://nixos.org/nix/install | sh
  fi

  if [[ -n $XDG_CONFIG_HOME ]]; then
    export XDG_CONFIG_HOME=$HOME/.config
  fi
  # nix-shell default.nix  \
  #           --substituters "https://cache.nixos.org" \
  #           --run \
  #           'bash scripts/shellcheck.sh'
  nix-shell default.nix  \
            --substituters "https://cache.nixos.org" \
            --run \
            "bash scripts/run-ansible.sh"
fi
