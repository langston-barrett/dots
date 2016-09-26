#!/usr/bin/env sh
nix-shell default.nix --run \
          'ansible-playbook --forks 100 \
                            -i inventory \
                            -e ansible_python_interpreter=$(which python) \
                            dots.yml'
