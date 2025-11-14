#!/bin/sh
# ^-- for shellcheck

# TODO
# shellcheck disable=all

dockeri() {
  if [[ ${OSTYPE} == darwin* ]]; then
    docker run \
      --rm \
      --interactive \
      --tty \
      "${@}"
  else
    sudo -g docker docker run \
      --rm \
      --interactive \
      --tty \
      "${@}"
  fi
}

docker-here() {
  dockeri \
    --mount "type=bind,src=${PWD},dst=/work" \
    --workdir /work \
    "${@}"
}

docker-dev() {
  docker-here \
    --env "PROMPT_EXTRA=${1} : " \
    --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
    --mount type=bind,readonly=true,src=$HOME/.config/bash,dst=/root/.config/bash \
    --mount type=bind,readonly=true,src=$HOME/code/dots/files/bashrc,dst=/root/.bashrc \
    --mount type=bind,readonly=true,src=$HOME/.config/sh.d,dst=/root/.config/sh.d \
    "${@}"
}

docker-pull-tag() {
  sudo -g docker docker pull "${1}:${3:-main}"
  sudo -g docker docker tag "${1}:${3:-main}" "${2}"
}
