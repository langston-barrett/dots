#!/usr/bin/env zsh

# this file is sourced last

installed() { command -v "$1" >/dev/null 2>&1; }
if ! installed zbr; then
  zbr-space() { zle .self-insert; }
  zle -N zbr-space
  zbr-ret() { zle accept-line; }
  zle -N zbr-ret
  zbr-hint() { }
  zle -N zbr-hint
fi

# TODO: hint system

kludge-expand() {
  out=$(env RUST_BACKTRACE=1 kludge expand -- "${LBUFFER}" "${RBUFFER}")
  if [ "${?}" -eq 0 ] && [ -n "${out}" ]; then
    BUFFER=${out}
    CURSOR=${#BUFFER}
  fi
}
zle -N kludge-expand

kludge-hint() {
  if [[ -n ${BUFFER% } ]]; then
    out=$(env RUST_BACKTRACE=1 kludge expand --hint -- "${LBUFFER}" "${RBUFFER}")
    if [[ -n $out ]]; then
      newline=$'\n'
      zle -M "$(printf "%s%s" "${newline}" "${out}")"
    else
      zbr-hint
    fi
  fi
}
zle -N kludge-hint

expand-space() {
  zle kludge-expand
  zle kludge-hint
  zle zbr-space
}
zle -N expand-space

expand-ret() {
  zle kludge-expand
  zle zbr-ret
}
zle -N expand-ret

bindkey -M emacs " " expand-space
bindkey -M viins " " expand-space
bindkey -M emacs "^M" expand-ret
bindkey -M viins "^M" expand-ret

# control-space is a normal space
bindkey -M emacs "^ " magic-space
bindkey -M viins "^ " magic-space

function zle-line-pre-redraw() {
  zle kludge-hint
}
zle -N zle-line-pre-redraw
