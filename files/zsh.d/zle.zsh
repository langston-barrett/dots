#!/usr/bin/env zsh

run_porcelain() {
  porcelain "$HOME/code/+personal/porcelain/conf"
  zle -Rc
  zle reset-prompt
}
zle -N run_porcelain

bindkey '^F' run_porcelain

# Search history with fzf with ctrl-R ctrl-R
# https://github.com/junegunn/fzf/wiki/Examples#command-history
fzf-history() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -r 's/ *[0-9]*\*? *//' | sed -r 's/\\/\\\\/g')
}
zle -N fzf-history
bindkey '^R^R' fzf-history
