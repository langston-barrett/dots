#!/usr/bin/env bash

array() {
  if [[ -n "${ZSH_NAME}" ]]; then
    IFS="${1:-${IFS}}" read -rA arr <<< "${2}"
  else
    IFS="${1:-${IFS}}" read -ra arr <<< "${2}"
  fi
  for i in "${arr[@]}"; do
    printf '%s\n' "${i}"
  done
}

