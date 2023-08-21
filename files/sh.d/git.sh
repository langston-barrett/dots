#!/bin/sh

export CARGO_NET_GIT_FETCH_WITH_CLI=true

git_list_add_targets_helper() {
  modified="$(git ls-files --directory --deleted --modified --others)"
  for f in ${modified}; do
    while [ -n "${f}" ] && [ "${f}" != . ]; do
      printf "%s\n" "${f}"
      f="$(dirname "${f}")"
    done
  done
}

git_list_add_targets() {
  if ! [[ -d "${PROJECT_ROOT}/.git" ]]; then
    return
  fi
  git_list_add_targets_helper | sort | uniq
}

git_list_checkout_targets() {
  if ! [[ -d "${PROJECT_ROOT}/.git" ]]; then
    return
  fi
  printf \
    "%s\n%s\n%s" \
    "$(git branch --all --format='%(refname:short)')" \
    "$(git_list_remote_branches)" \
    "$(git tag -l)" | sort | uniq
}
