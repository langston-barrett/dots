#!/usr/bin/env zsh

alias jq_clipboard="xsel -ob | jq | xsel -ib"

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
  rand=$(tr -dc 'a-zA-Z0-9' < /dev/urandom | fold -w 16 | head -n 1)
  git checkout -b "$rand" "$1"
  git commit -m "$2"
  git push
}

## Git
alias ga='git add'
alias gb='git branch'
alias gbD='git branch -D'
alias gc='git checkout'
alias gcl='git clone --depth 20'
alias gcm='git commit -m'
alias gca='EDITOR=vi git commit --amend'
alias gcb='git checkout -b'
alias gd='git diff'
alias gds='git diff --cached'
alias gdm='git diff master'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gFp='git pull origin'
alias gFu='git pull upstream'
alias gm='git merge'
alias gmum='git merge upstream/master'
alias gp='git checkout master && git pull && git checkout -'
alias gpum='git pull upstream master'
alias gPp='git push -u origin'
alias gPf='git push --force-with-lease'
alias gr='git reset'
alias grhm='git reset --hard origin/master'
alias gri='EDITOR=vi git rebase -i'
alias grv='git remote -v'
alias gs='git status'
alias gss='git status --short'
alias gwl='git worktree list'
alias gwa='git worktree add'
alias gwm='git worktree move'
alias gwr='git worktree remove'

function github_clone { git clone "https://github.com/${1}"; }
function git_clone_mine { git clone "https://github.com/siddharthist/${1}"; }

# https://stackoverflow.com/questions/3231804/in-bash-how-to-add-are-you-sure-y-n-to-any-command-or-alias#32708121
prompt_confirm() {
  while true; do
    read -r -n 1 -p "${1:-Continue?} [y/n]: " REPLY
    case $REPLY in
      [yY]) echo ; return 0 ;;
      [nN]) echo ; return 1 ;;
      *) printf " \033[31m %s \n\033[0m" "invalid input"
    esac
  done
}

function git_delete_untracked {
  for f in $(git ls-files --others --exclude-standard); do
    if prompt_confirm "Want to delete $f?"; then
      tp "$f"
    fi
  done
}

function git() {
  if [[ "$1" == "push" ]]; then
    branch=$(git rev-parse --abbrev-ref HEAD)
    if [[ $branch == "master" ]]; then
      case "$(basename "$(pwd)")" in
        "sfe") echo "Refusing to push to master" ;;
        "renovate") echo "Refusing to push to master" ;;
        "parameterized-utils") echo "Refusing to push to master" ;;
        "chess") echo "Refusing to push to master" ;;
        *) command git "$@"
      esac
    else
      command git "$@"
    fi
  else
    command git "$@"
  fi
}


# systemd

alias sys="sudo systemctl";
alias syss="sudo systemctl status";
alias sysr="sudo systemctl restart";
alias sysu="systemctl --user";
alias sysus="systemctl --user status";
alias sysur="systemctl --user restart";

## Nix
alias nb='nix-build'
alias nba='nix-build -A'
alias ns='nix-shell'
alias nsr='nix-shell --run'
alias nsp='nix-shell --pure'
alias nspr='nix-shell --pure --run'

# TODO: replace with "nix run || nix log"
alias nsrzsh='nix-shell --run "exec zsh"'
alias nz='nsrzsh || nsrzsh nix/shell.nix || nsrzsh tools/shell.nix'

run_from_unstable () {
  nix-shell -p "with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) { }; $1" --run "$1 & disown"

}

alias kmonad-minidox='cd /tmp && sudo echo && sudo nohup kmonad ~/code/dots/files/kmonad/minidox.kbd & disown'
alias kmonad-mini='z kmon && sudo echo && sudo kmonad ~/code/dots/files/kmonad/mini.kbd & disown'

## Generic

alias ag='ag --path-to-ignore ~/code/dots/files/agignore'
alias makej='make -j$(nproc)'
alias docker='sudo -g docker docker'
alias lock='systemctl start physlock'

GPG_TTY=$(tty)
export GPG_TTY

setopt HIST_IGNORE_DUPS     # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS    # Do not display a line previously found.
setopt HIST_IGNORE_SPACE    # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS    # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS   # Remove superfluous blanks before recording entry.

# RACK

alias rack-inabox='docker pull interran/rack-box:v2.9 && docker run --rm --detach -p 80:80 -p 3030:3030 -p 12050-12092:12050-12092 interran/rack-box:v2.9'

# Mate

mate-dev-run() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,srd=~/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         bash -c "$@"
}

mate-img-run() {
  docker pull "${2:-artifactory.galois.com:5004/mate-dev:master}"
  docker run \
         --rm \
         --net=host \
         --mount type=bind,srd=~/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         "${2:-artifactory.galois.com:5004/mate-dev:master}" \
         bash -c "${1:-bash}"
}

mate-shake() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,srd=~/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         ./shake.sh -j$(nproc) -- "$1" -- "${@:2}"

}

mate-pytest-one() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,srd=~/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         ./shake.sh -j$(nproc) -- pytests -- -vv -x -k "$1"
}

mate-pytest-debug() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,srd=~/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         bash -c "source source.sh && cd frontend && pytest -vv --pdb -n0 -x -k $1"
}

mate-pytest-one-integration() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,srd=~/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         bash -c "MATE_INTEGRATION_TESTS=1 ./shake.sh -j$(nproc) -- pytests -- --show-capture=all -vv -x -k $1"
}

# use mate-shake bench

alias mate-docker-pull='docker pull artifactory.galois.com:5004/mate-dev:master && docker tag artifactory.galois.com:5004/mate-dev:master mate-dev && docker pull artifactory.galois.com:5004/mate-dist:master && docker tag artifactory.galois.com:5004/mate-dist:master mate-dist && docker image prune'
alias mate-clean-submodule-integration-framework='pushd submodules/integration_framework && sudo git clean -xdf && sudo git reset --hard HEAD && popd'

# temporary

alias restart_qute='kill -9 $(pgrep qutebrowser) && qutebrowser 2>&1 > /dev/null & disown'
alias restart_steam='kill -9 $(pgrep steam) && steam 2>&1 > /dev/null & disown'
