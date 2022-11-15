#!/usr/bin/env bash

# Inspiration:
# https://github.com/junegunn/fzf-git.sh/blob/main/fzf-git.sh
# https://github.com/wfxr/forgit
zshfzf() {
   fzf --height=10% --layout=reverse --prompt='' --info=hidden "${@}"
}

# TODO:
# - fetch
# - tag -d
# - reset
# - remote rm

fzf-git-add() {
   git_list_add_targets | zshfzf --preview="git diff --no-ext-diff --color=always -- {-1} | sed 1,4d"
}

fzf-git-checkout() {
  git_list_checkout_targets | zshfzf --preview="git log --oneline --color=always -- {-1}"
}
