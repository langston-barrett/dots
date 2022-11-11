#!/usr/bin/env bash
# The above is for ShellCheck

find_project_root() {
  root=$(realpath "${1:-${PWD}}")
  while true; do
    if [[ ${root} == "." ]]; then
      printf "%s\n" "ERROR"
      break
    fi
    if [[ ${root} == / ]] || [[ -f ${root}/.projectile ]] || [[ -d ${root}/.git  ]] || [[ -d ${root}/cabal.project  ]]; then
      break
    fi
    root=$(dirname "${root}")
  done
  printf "%s" "${root}"
}

export PROJECT_ROOT=/
set_project_root() {
  export PROJECT_ROOT=$(find_project_root)
}

autoload -Uz add-zsh-hook
add-zsh-hook chpwd set_project_root
