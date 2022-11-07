#!/usr/bin/env bash

set -e

installed() { command -v "$1" >/dev/null 2>&1; }
bail_if_not_installed() {
  if ! installed "$1"; then
    echo "$0 requires $1, but it isn't on the PATH."
    exit 1
  fi
}

bail_if_not_installed ansible-playbook
bail_if_not_installed python

ansible-playbook \
  --forks 100 \
  -i inventory \
  -e "ansible_python_interpreter=$(which python)" \
  -e "username=$(whoami)" \
  -e "@${1:-presets/${DOTS_PRESET:-default}.json}" \
  dots.yml
