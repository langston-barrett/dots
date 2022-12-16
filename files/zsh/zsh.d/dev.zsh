#!/usr/bin/env zsh

export PATH=$PATH:$HOME/.cargo/bin
export PATH=$PATH:$HOME/.config/bin

eval $(lesspipe.sh)

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

## Generic

GPG_TTY=$(tty)
export GPG_TTY

# Docker

# alias docker='sudo -g docker docker'
# alias docker-compose='sudo -g docker docker-compose'

docker-ssh() {
    images=$(docker ps --format "{{.ID}} {{.Image}}")
    image=$(fzf <<< "${images}" | awk '{ print $1 }')
    [[ -n "${image}" ]] && docker exec -it "${image}" bash
}

# MATE

mate-run() {
  docker run \
         --rm \
         --net=host \
         --mount type=tmpfs,dst=/tmp \
         --env "PROMPT_EXTRA=${1} : " \
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$HOME/code/dots/files/bash.d,dst=/root/.bash.d \
         --mount type=bind,src=$HOME/code/dots/files/bashrc,dst=/root/.bashrc \
         --mount type=bind,src=$PWD,dst=/mate \
         --workdir=/mate \
         --interactive \
         --tty \
         "${1}" \
         bash -c "${*[2, -1]}"
}

mate-dev-run() { mate-run mate-dev "${*:-bash}"; }
mate-oss-run() { mate-run mate-dev "${*:-bash}"; }
mate-dist-run() { mate-run mate-dist "${*:-bash}"; }

mate-shake() {
  mate-run \
    mate-dev \
    "./shake.sh -j$(nproc) -- \"${1}\" -- \"${@:2}\""
}

mate-pytest-one() {
  mate-run \
    mate-dev \
    "./shake.sh -j$(nproc) -- pytests -- -vv -x -k \"${1}\""
}

mate-pytest-debug() {
  mate-run \
    mate-dev \
    "./shake.sh -j$(nproc) -- pytests -- -vv -x -k \"${1}\""
}

mate-pytest-debug() {
  mate-run \
    mate-dev \
    "bash -c \"source source.sh && cd frontend && pytest --capture=no -vv --pdb -n0 -x -k ${1}\""
}

mate-pytest-debug-integration() {
  mate-run \
    mate-dev \
    "bash -c \"source source.sh && cd frontend && MATE_INTEGRATION_TESTS=1 pytest -vv --pdb -n0 -x -k ${1}\""
}

mate-pytest-one-integration() {
  mate-run \
    mate-dev \
    "bash -c \"MATE_INTEGRATION_TESTS=1 ./shake.sh -j$(nproc) -- pytests -- --show-capture=all -vv -x -k ${1}\""
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
    mate-run \
      mate-dev \
      "bash -c \"source source.sh && mate -w ${ws} compile frontend/test/programs/example_1.c && mate -w ${ws} build && mate -w ${ws} shell\""
}

mate-build-challenge() {
  mate-run "${2:-mate-dev}" \
           bash -c \
           "rm -rf w || true; \
            source source.sh; \
            mate -w w compile submodules/mate-tests/tests/${1:?'Challenge name required'}/challenge_src/; \
            mate -w w build"
  cp w/canonical.*.bc "${1}.bc"
}

mate-dist-build() {
  mate-docker-pull
  mate-shake bdist
  docker build --tag "${1}" --target=dist .
}

# use mate-shake bench

poly-run() {
  echo "${*[1, -1]}"
  docker run \
         --rm \
         --entrypoint "" \
         --mount type=tmpfs,dst=/tmp \
         --mount type=bind,src=$HOME/.bash_history,dst=/root/.bash_history \
         --mount type=bind,src=$HOME/code/dots/files/bash.d,dst=/root/.bash.d \
         --mount type=bind,src=$HOME/code/dots/files/bashrc,dst=/root/.bashrc \
         --mount type=bind,src=$PWD,dst=/polymorph \
         --workdir=/polymorph \
         --interactive \
         --tty \
         polymorph-dev \
         bash -c "${*[1, -1]}"
}

poly-shake() {
  poly-run \
    "./shake.sh -j$(nproc) -- \"${1}\" -- \"${@:2}\""
}
