fancy-ctrl-z () {
  emulate -LR zsh
  if [[ ${#BUFFER} -eq 0 ]]; then
    bg
    zle_append_to_buffer "fg && "
  else
    zle push-input
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z
