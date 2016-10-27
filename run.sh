#!/usr/bin/env sh
export XDG_CONFIG_HOME=$HOME/.config
nix-shell default.nix --run \
          'ansible-playbook --forks 100 \
                            -i inventory \
                            -e ansible_python_interpreter=$(which python) \
                            dots.yml'
