#!/usr/bin/env zsh

typeset -g VI_KEYMAP=main

vi_mode_set_cursor() {
  if [[ ${1:-${VI_KEYMAP}} == vicmd ]]; then
    ansi_block_cursor
    ansi_no_cursor_blink
  else
    ansi_bar_cursor
    ansi_no_cursor_blink
  fi

}

zle-vi-set-mode() {
  typeset -g VI_KEYMAP=${1:-main}
  vi_mode_set_cursor
  zle reset-prompt
  zle -R
}

# overrides for built-in widgets
zle-keymap-select() { zle-vi-set-mode "${KEYMAP}"; }
zle-line-init() { zle-vi-set-mode "${KEYMAP}"; }
zle-line-finish() { zle-vi-set-mode "${KEYMAP}"; }
zle -N zle-keymap-select
zle -N zle-line-init
zle -N zle-line-finish

# (overrides built-in)
vi-accept-line() {
  VI_KEYMAP=main
  zle accept-line
}
zle -N vi-accept-line

function vi_mode_prompt() {
  local mode="${1:-${VI_KEYMAP:-main}}"
  case "${mode}" in
     main) printf "%s\n" "%{$fg[green]%}%3d%{$reset_color%}" ;;
     viins) printf "%s\n" "%{$fg[green]%}%3d%{$reset_color%}" ;;
     command) printf "%s\n" "%{$fg[blue]%}%3d%{$reset_color%}" ;;
     spacezle) printf "%s\n" "%{$fg[red]%}%3d%{$reset_color%}" ;;
     vicmd) printf "%s\n" "%{$fg[blue]%}%3d%{$reset_color%}" ;;
     *) printf "vi mode=${mode}?\n" ;;
   esac
}
PS1=${PS1:s/%3d/'$(vi_mode_prompt)'}
