#!/usr/bin/env zsh

[[ -o interactive ]] || return

# $EPOCHSECONDS
zmodload zsh/datetime || return

kludge_begin() {
    zbell_timestamp=${EPOCHSECONDS:-0}
    zbell_lastcmd="${1}"
    kludge hook begin --time "${zbell_timestamp}" ${1}
}

kludge_end() {
    kludge hook end \
      --begin "${zbell_timestamp:-${EPOCHSECONDS:-0}}" \
      --end "${EPOCHSECONDS:-0}" \
      "${zbell_lastcmd}"
}

autoload -Uz add-zsh-hook || return
add-zsh-hook preexec kludge_begin
add-zsh-hook precmd kludge_end
