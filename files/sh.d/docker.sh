docker-here() {
  docker run \
    --rm \
    --interactive \
    --tty \
    --mount type=bind,src=$PWD,dst=/work \
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
  docker pull "${1}:${3:-main}"
  docker tag "${1}:${3:-main}" "${2}"
}

ccl-docker-pull() {
  docker-pull-tag ghcr.io/galoisinc/cclyzerpp-dev cclyzer-dev "${1}"
  docker-pull-tag ghcr.io/galoisinc/cclyzerpp-dist cclyzer-dist "${1}"
}

mate-docker-pull() {
  docker-pull-tag ghcr.io/galoisinc/mate-dev mate-dev "${1}"
  docker-pull-tag ghcr.io/galoisinc/mate-dist mate-dist "${1}"
}

docker-pull-tag() {
  docker pull "${1}:${3:-main}"
  docker tag "${1}:${3:-main}" "${2}"
}

polymorph-docker-pull() {
  docker-pull-tag artifactory.galois.com:5016/polymorph/dist polymorph-dist "${1}"
  docker-pull-tag artifactory.galois.com:5016/polymorph/dev polymorph-dev "${1}"
  docker-pull-tag artifactory.galois.com:5016/polymorph/vscode-integration-test polymorph-vscode-integration-test "${1}"
}

alias d='sudo -g docker zsh'
