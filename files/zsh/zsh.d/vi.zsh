#!/usr/bin/env zsh

# http://stratus3d.com/blog/2017/10/26/better-vi-mode-in-zshell/

# Unbind all sequences starting with ESC
#
# See man zshzle.
#
# Unfortunately, unbinds arrow keys...
#
# bindkey -rpM viins '^['
# bindkey -M viins '^? ' vi-forward-char

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
#
# Also be sure to set this in tmux.conf: `set -s escape-time 0`.
export KEYTIMEOUT=20

vi_mode_set_cursor() {
  if [[ ${1:-${VI_KEYMAP}} == vicmd ]]; then
    ansi_block_cursor
    ansi_no_cursor_blink
  else
    ansi_bar_cursor
    ansi_no_cursor_blink
  fi
  
}

# Updates editor information when the keymap changes.
typeset -g VI_KEYMAP=main
zle-vi-set-mode() {
  typeset -g VI_KEYMAP=${1:-main}
  vi_mode_set_cursor
  zle reset-prompt
  zle -R
}

function zle-keymap-select() {
  zle-vi-set-mode "${KEYMAP}"
}
zle -N zle-keymap-select

function zle-line-init() {
  zle-vi-set-mode "${KEYMAP}"
  # Always start in normal mode:
  # zle -K vicmd
}
zle -N zle-line-init

function zle-line-finish() {
  zle-vi-set-mode "${KEYMAP}"
}
zle -N zle-line-finish

function vi-accept-line() {
  VI_KEYMAP=main
  zle accept-line
}

zle -N vi-accept-line

bindkey -v

# if mode indicator wasn't setup by theme, define default
if [[ "${MODE_INDICATOR}" == "" ]]; then
  MODE_INDICATOR="%{$fg_bold[red]%}<%{$fg[red]%}<<%{$reset_color%}"
fi

# function vi_mode_prompt_info() {
#  case "${1:-${VI_KEYMAP:-main}}" in
#     main) printf "%s\n" "%{$fg[yellow]%}i%{$reset_color%}" ;;
#     viins) printf "%s\n" "%{$fg[yellow]%}i%{$reset_color%}" ;;
#     isearch) printf "isearch\n" ;;
#     command) printf "%s\n" "%{$fg[blue]%}n%{$reset_color%}" ;;
#     vicmd) printf "%s\n" "%{$fg[blue]%}n%{$reset_color%}" ;;
#     spacezle) printf "%s\n" "%{$fg[green]%}n%{$reset_color%}" ;;
#     visual) printf "sel\n" ;;
#     viopp) printf "opp\n" ;;
#     *) printf "???\n" ;;
#   esac
# }

function use-j-k() {
  if [[ $((${SECONDS} - ${last:-0})) -gt 10 ]]; then
    notify-send "you can use ctrl-j and ctrl-k"
    last=${SECONDS}
  fi
}
# zle -N use-j-k
# bindkey '^[OA' use-j-k
# bindkey '^[OB' use-j-k

zle-z() {
  zle_append_to_buffer "z "
  zle vi-insert
}
zle -N zle-z
bindkey -M vicmd 'z' zle-z

