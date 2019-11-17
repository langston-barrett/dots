#!/usr/bin/env zsh

source ~/.zsh.d/zaw/zaw.zsh
# CTRL-SPC CTRL-SPC
bindkey '^ ^ ' zaw
# CTRL-R is the Bash mnemonic for "history"
bindkey '^R^R' zaw-history
# CTRL-B for "branches"
bindkey '^R B' zaw-git-branches
