#!/usr/bin/env zsh

# shellcheck disable=all

# this file is sourced last

# ref: kludge-expand

installed() { command -v "$1" >/dev/null 2>&1; }
if ! installed zbr; then
  zbr-space() { zle .self-insert; }
  zle -N zbr-space
  zbr-ret() { zle accept-line; }
  zle -N zbr-ret
  zbr-hint() { }
  zle -N zbr-hint
fi

kludge-space() {
  out=$(env RUST_BACKTRACE=1 kludge zsh expand -- "${LBUFFER}" "${RBUFFER}" </dev/null)
  if [ "${?}" -eq 0 ] && [ -n "${out}" ]; then
    BUFFER=${out/•/}
    CURSOR=${out[(Ie)•]}
    if [[ ${CURSOR} -ne ${#BUFFER} ]]; then
      CURSOR=$((CURSOR-1))
    fi
  fi
}
zle -N kludge-space

kludge-ret() {
  out=$(env RUST_BACKTRACE=1 kludge zsh expand --enter -- "${BUFFER}" "" </dev/null)
  if [ "${?}" -eq 0 ] && [ -n "${out}" ]; then
    BUFFER=${out/•/}
  fi
}
zle -N kludge-ret

kludge-hint() {
  if [[ -n ${BUFFER% } ]]; then
    out=$(env RUST_BACKTRACE=1 kludge zsh expand --hint -- "${LBUFFER}" "${RBUFFER}" </dev/null)
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
  old=${BUFFER}
  zle kludge-space
  zle kludge-hint
  if [[ ${BUFFER} == ${old} ]]; then
    zle zbr-space
  else
    zle .self-insert
  fi
}
zle -N expand-space

expand-ret() {
  old=${BUFFER}
  zle kludge-ret
  if [[ ${BUFFER} == ${old} ]]; then
    zle zbr-ret
  else
    zle accept-line
  fi
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
