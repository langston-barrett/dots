#!/usr/bin/env zsh

# Delete all merged git branches. Use caution, and only use on master.
# http://goo.gl/r9Bos0
clean_merged() {
  git branch --merged | grep -v "\*" \
    | grep -v master \
    | xargs -n 1 git branch -d
}

## Git
alias ga='git add'
alias gb='git branch'
alias gc='git checkout'
alias gcl='git clone --depth 20'
alias gcm='git commit -m'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdm='git diff master'
alias gf='git fetch'
alias gm='git merge'
alias gp='git checkout master && git pull && git checkout -'
alias gpo='git push origin'
alias gr='git reset'
alias grhm='git reset --hard origin/master'
alias gri='git rebase -i'
alias gs='git status'

## Nix
alias ns='nix-shell'
alias nsr='nix-shell --run'
alias nsrzsh='nix-shell --run "exec zsh"'

## Generic

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
