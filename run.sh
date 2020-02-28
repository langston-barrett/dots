#!/usr/bin/env bash

set -e

installed() { command -v "$1" >/dev/null 2>&1; }
bail_if_not_installed() {
  if ! installed "$1"; then
    echo "$0 requires $1, but it isn't on the PATH."
    exit 1
  fi
}

bail_if_not_installed python

if ! installed nix; then
  bail_if_not_installed curl
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
          'ansible-playbook --forks 100 \
                            -i inventory \
                            -e ansible_python_interpreter=$(which python) \
                            dots.yml'
