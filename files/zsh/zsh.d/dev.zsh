#!/usr/bin/env zsh

export PATH=$PATH:$HOME/.cargo/bin
export PATH=$PATH:$HOME/.config/bin

installed() { command -v "$1" >/dev/null 2>&1; }
if installed lesspipe.sh; then
  eval $(lesspipe.sh)
fi

# Docker

docker-ssh() {
    images=$(docker ps --format "{{.ID}} {{.Image}}")
    image=$(fzf <<< "${images}" | awk '{ print $1 }')
    [[ -n "${image}" ]] && docker exec -it "${image}" bash
}
