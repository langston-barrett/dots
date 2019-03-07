#!/usr/bin/env zsh

# Delete all merged git branches. Use caution, and only use on master.
# http://goo.gl/r9Bos0
clean_merged() {
  git branch --merged | grep -v "\*" \
    | grep -v master \
    | xargs -n 1 git branch -d
}

# Commit staged changes to a new branch and push it
# Args:
# 1. Base branch
# 2. Message
commit_to_new_branch() {
  rand=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)
  git checkout -b "$rand" "$1"
  git commit -m "$2"
  git push
}

edit_markdown() {
  emacsclient --create-frame "$1" & disown
  grip "$1" localhost:8199 & disown
  firefox localhost:8199
}

## Git
alias ga='git add'
alias gb='git branch'
alias gbD='git branch -D'
alias gc='git checkout'
alias gcl='git clone --depth 20'
alias gcm='git commit -m'
alias gcb='git checkout -b'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdm='git diff master'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gm='git merge'
alias gmum='git merge upstream/master'
alias gp='git checkout master && git pull && git checkout -'
alias gpum='git pull upstream master'
alias gpo='git push -u origin'
alias gr='git reset'
alias grhm='git reset --hard origin/master'
alias gri='git rebase -i'
alias grv='git remote -v'
alias gs='git status'

function github_clone { git clone "https://github.com/$1" }
function git_clone_mine { git clone "https://github.com/siddharthist/$1" }

## Nix
alias nb='nix-build'
alias nba='nix-build -A'
alias ns='nix-shell'
alias nsr='nix-shell --run'
alias nsp='nix-shell --pure'
alias nspr='nix-shell --pure --run'

# TODO: replace with "nix run || nix log"
alias nsrzsh='nix-shell --run "exec zsh"'
alias nz='nix-shell --run "exec zsh" || nix-shell nix/shell.nix --run "exec zsh"'

## Generic

alias ag='ag --path-to-ignore ~/code/dots/files/agignore'
alias makej='make -j$(nproc)'

proj_test () {
  test_cmd="make test"

  if [[ -f setup.py ]]; then
    test_cmd="./setup.py test"
  elif [[ -f *.cabal ]]; then
    test_cmd="cabal test"
  elif [[ -f stack.yaml ]]; then
    test_cmd="stack test"
  fi

  if [[ -z "$IN_NIX_SHELL" ]] && ([[ -f default.nix ]] || [[ -f shell.nix ]]); then
    nix-shell --run "$test_cmd"
  else
    $test_cmd
  fi
}

GPG_TTY=$(tty)
export GPG_TTY

setopt HIST_IGNORE_DUPS     # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS    # Do not display a line previously found.
setopt HIST_IGNORE_SPACE    # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS    # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS   # Remove superfluous blanks before recording entry.
