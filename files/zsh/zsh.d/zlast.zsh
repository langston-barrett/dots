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

# Strip a leading shell-pipeline prefix off LBUFFER, mirroring kludge's
# clean_buf in expand.rs. Sets REPLY=remainder, REPLY2=prefix.
kludge-clean-buf() {
  local lbuf="${1}"
  local prefix=""
  local delim
  for delim in ' || ' ' && ' '; ' '| ' '|& '; do
    if [[ ${lbuf} == *${delim}* ]]; then
      prefix="${lbuf%${delim}*}${delim}"
      lbuf="${lbuf##*${delim}}"
    fi
  done
  REPLY="${lbuf}"
  REPLY2="${prefix}"
}

kludge-apply() {
  local out="${1}"
  BUFFER=${out/•/}
  CURSOR=${out[(Ie)•]}
  if [[ ${CURSOR} -ne ${#BUFFER} ]]; then
    CURSOR=$((CURSOR-1))
  fi
}

kludge-space() {
  # Fast path: in-shell ANYWHERE lookup (no fork). Matches expand_anywhere
  # in expand.rs: only triggers when RBUFFER is empty.
  if [[ -z ${RBUFFER} ]] && (( ${+KLUDGE_ANYWHERE} )); then
    local REPLY REPLY2
    kludge-clean-buf "${LBUFFER}"
    if (( ${+KLUDGE_ANYWHERE[$REPLY]} )); then
      kludge-apply "${REPLY2}${KLUDGE_ANYWHERE[$REPLY]}"
      return
    fi
  fi
  # Miss: project / advanced expansions still need PWD, fork as before.
  out=$(env RUST_BACKTRACE=1 kludge zsh expand -- "${LBUFFER}" "${RBUFFER}" </dev/null)
  if [ "${?}" -eq 0 ] && [ -n "${out}" ]; then
    kludge-apply "${out}"
  fi
}
zle -N kludge-space

kludge-ret() {
  # Fast path: ANYWHERE exact match, then ANYWHERE_ENTER prefix match.
  if (( ${+KLUDGE_ANYWHERE} )); then
    local REPLY REPLY2
    kludge-clean-buf "${BUFFER}"
    if (( ${+KLUDGE_ANYWHERE[$REPLY]} )); then
      local out="${REPLY2}${KLUDGE_ANYWHERE[$REPLY]}"
      BUFFER=${out/•/}
      return
    fi
    local k
    for k in ${(k)KLUDGE_ANYWHERE_ENTER}; do
      if [[ ${REPLY} == ${k}\ * ]]; then
        local rest="${REPLY#${k} }"
        local long="${KLUDGE_ANYWHERE_ENTER[$k]}"
        BUFFER="${REPLY2}${long//_/${rest}}"
        return
      fi
    done
  fi
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
