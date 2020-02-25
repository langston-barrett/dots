#!/usr/bin/env bash

set -e

export XDG_CONFIG_HOME=$HOME/.config

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
