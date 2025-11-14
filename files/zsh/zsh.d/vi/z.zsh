#!/usr/bin/env zsh
# ^-- for shellcheck

# shellcheck disable=SC2034

append-to-buffer() {
  BUFFER+="${1}"
  CURSOR="${#BUFFER}"
  zle redisplay
}
zle-z() {
  append-to-buffer "z "
  zle vi-insert
}
zle -N zle-z
bindkey -M vicmd 'z' zle-z
