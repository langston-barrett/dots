#!/usr/bin/env bash

# TODO:
# - fetch
# - tag -d
# - reset
# - remote rm

fzf-git-add() {
   git_list_add_targets | pick --preview="git diff --no-ext-diff --color=always -- {-1} | sed 1,4d"
}

fzf-git-checkout() {
  git_list_checkout_targets | pick --preview="git log --oneline --color=always -- {-1}"
}
