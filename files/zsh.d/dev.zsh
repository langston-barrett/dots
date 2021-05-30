#!/usr/bin/env zsh

export PATH=$PATH:$HOME/.config/bin

cpr() { cp "${2}" "${1}"; }

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

sorted_diff() {
    file1=$(mktemp)
    file2=$(mktemp)
    bat "${1}" | sort > "${file1}"
    bat "${2}" | sort > "${file2}"
    diff "${file1}" "${file2}"
}

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

run_from_unstable () {
  nix-shell -p "with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) { }; $1" --run "$1 & disown"

}

## Generic

GPG_TTY=$(tty)
export GPG_TTY

setopt HIST_IGNORE_DUPS     # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS    # Do not display a line previously found.
setopt HIST_IGNORE_SPACE    # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS    # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS   # Remove superfluous blanks before recording entry.

# Docker

# alias docker='sudo -g docker docker'
# alias docker-compose='sudo -g docker docker-compose'

docker-ssh() {
    images=$(docker ps --format "{{.ID}} {{.Image}}")
    image=$(fzf <<< "${images}" | awk '{ print $1 }')
    [[ -n "${image}" ]] && docker exec -it "${image}" bash
}

# MATE

mate-dev-run() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --mount type=tmpfs,dst=/tmp \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         bash -c "${@:-bash}"
}

mate-dev-souffle-run() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev-souffle \
         bash -c "${@:-bash}"
}

mate-dev-souffle-run() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev-souffle \
         bash -c "$@"
}

mate-dist-run() {
    docker run \
           --rm \
           --net=host \
           --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
           --mount type=bind,src=$PWD,dst=/x \
           --workdir=/x \
           --interactive \
           --tty \
           mate-dist \
           bash -c "${@:-bash}"
}

mate-img-run() {
  docker pull "${2:-artifactory.galois.com:5004/mate-dev:master}"
  docker run \
         --rm \
         --net=host \
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
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
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
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
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         bash -c "source source.sh && cd frontend && pytest --capture=no -vv --pdb -n0 -x -k $1"
}

mate-pytest-debug-integration() {
    docker run \
           --rm \
           --net=host \
           --mount type=bind,src=$PWD,dst=/x \
           --workdir=/x \
           --interactive \
           --tty \
           mate-dev \
           bash -c "source source.sh && cd frontend && MATE_INTEGRATION_TESTS=1 pytest -vv --pdb -n0 -x -k $1"
}

mate-pytest-one-integration() {
  docker run \
         --rm \
         --net=host \
         --mount type=bind,src=$PWD,dst=/x \
         --workdir=/x \
         --interactive \
         --tty \
         mate-dev \
         bash -c "MATE_INTEGRATION_TESTS=1 ./shake.sh -j$(nproc) -- pytests -- --show-capture=all -vv -x -k $1"
}

mate-compose() {
    sudo -g docker docker-compose -f docker-compose.yml -f docker-compose.override.yml "${@}"
}

mate-dev-docker-ssh() {
    docker exec -w /x -it $(docker ps | grep mate-dev | awk '{print $1}') "${@}"
}

mate-dist-docker-ssh() {
    docker exec -it $(docker ps | grep mate-dist | awk '{print $1}') "${@}"
}

mate-lint-entr() {
    fd . \
       --extension py \
       --extension cpp \
       --extension c \
       --extension h \
       --extension hpp \
       --exclude submodules \
       --exclude misc \
       | entr -c -s 'source ~/.zsh.d/dev.zsh && mate-shake lint'
}

mate-lint-entr-py() {
    fd . \
       --extension py \
       --exclude submodules \
       --exclude misc \
        | entr -c -s 'source ~/.zsh.d/dev.zsh && mate-shake lint-py'
}

mate-shell-example-1() {
    mate-shake build
    ws=deleteme.workspace
    \rm -rf "${ws}"
    docker run \
           --rm \
           --net=host \
           --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
           --mount type=bind,src=$PWD,dst=/x \
           --workdir=/x \
           --interactive \
           --tty \
           mate-dev \
           bash -c "source source.sh && mate -w ${ws} compile frontend/test/programs/example_1.c && mate -w ${ws} build && mate -w ${ws} shell"
}

# use mate-shake bench
