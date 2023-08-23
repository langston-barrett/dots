export RUST_BACKTRACE=1

function kludge-hint() {
  if [[ -n ${BUFFER% } ]]; then
    out=$(printf "\n" && env RUST_BACKTRACE=1 kludge zle hint --max 5 $HOME/code/dots/kludge/conf.toml "${BUFFER% }")
    if [[ -n $out ]]; then
      zle -M "${out}"
    else
      zle -M ""
    fi
  fi
}

function kludge-space() {
  if [[ "${BUFFER% }" == "${BUFFER}" ]] || [[ "${CURSOR}" != "${#BUFFER}" ]]; then
    out=$(env RUST_BACKTRACE=1 kludge zle expand $HOME/code/dots/kludge/conf.toml "${LBUFFER}" "${RBUFFER}")
    if [ "${?}" -eq 0 ] && [ -n "${out}" ]; then
      BUFFER=${out}
      CURSOR=${#BUFFER}
    else
      zle .self-insert
    fi
  fi
  kludge-hint
}

# space
zle -N kludge-space
bindkey -M emacs " " kludge-space
bindkey -M viins " " kludge-space

# control-space is a normal space
bindkey -M emacs "^ " magic-space
bindkey -M viins "^ " magic-space

function zle-line-pre-redraw() {
  kludge-hint
}
zle -N zle-line-pre-redraw
