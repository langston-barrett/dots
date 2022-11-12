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

# TODO: Allow setting custom aliases
set_project_root() {
  export PROJECT_ROOT=$(find_project_root)
  
  if [[ -f ${PROJECT_ROOT}/Makefile ]] || [[ -f ${PWD}/Makefile ]]; then
    printf "Make project\n"
    alias a='make fmt && make lint && make test'
    alias b='make'
    alias f='make fmt'
    alias t='make test'
  elif [[ -f ${PROJECT_ROOT}/cabal.project ]] || [[ -f ${PWD}/cabal.project ]]; then
    printf "Cabal project\n"
    alias a='hlint src && cabal build && cabal test'
    alias b='cabal build'
    alias l='hlint src'
    alias t='cabal test'
  elif [[ -f ${PROJECT_ROOT}/Cargo.toml ]] || [[ -f ${PWD}/Cargo.toml ]] ; then
    printf "Cargo project\n"
    alias a=''
    alias b='cargo'
    alias f='cargo fmt'
    alias l='cargo clippy'
    alias t='cargo test'
  else
    unalias 'b' 2> /dev/null
    unalias 'f' 2> /dev/null
    unalias 'l' 2> /dev/null
    unalias 't' 2> /dev/null
  fi
}

autoload -Uz add-zsh-hook
add-zsh-hook chpwd set_project_root
set_project_root
