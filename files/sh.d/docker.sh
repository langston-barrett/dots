docker-here() {
  docker run --rm -it --mount type=bind,src=$PWD,target=/work --workdir /work "${@}"
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

polymorph-docker-pull() {
  docker-pull-tag artifactory.galois.com:5016/polymorph/dist polymorph-dist "${1}"
  docker-pull-tag artifactory.galois.com:5016/polymorph/dev polymorph-dev "${1}"
  docker-pull-tag artifactory.galois.com:5016/polymorph/vscode-integration-test polymorph-vscode-integration-test "${1}"
}

alias d='sudo -g docker zsh'
