#!/usr/bin/env zsh

# http://stratus3d.com/blog/2017/10/26/better-vi-mode-in-zshell/

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
export KEYTIMEOUT=20

# Updates editor information when the keymap changes.
typeset -g VI_KEYMAP=main
function zle-vi-set-mode() {
  typeset -g VI_KEYMAP=${1:-main}
  zle reset-prompt
  zle -R
}

function zle-keymap-select() {
  zle-vi-set-mode "${KEYMAP}"
}
zle -N zle-keymap-select

function zle-line-init() {
  zle-vi-set-mode "${KEYMAP}"
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

function vi_mode_prompt_info() {
 case "${1:-${VI_KEYMAP:-main}}" in
    main) printf "%s\n" "%{$fg[yellow]%}i%{$reset_color%}" ;;
    viins) printf "%s\n" "%{$fg[yellow]%}i%{$reset_color%}" ;;
    isearch) printf "isearch\n" ;;
    command) printf "%s\n" "%{$fg[blue]%}n%{$reset_color%}" ;;
    vicmd) printf "%s\n" "%{$fg[blue]%}n%{$reset_color%}" ;;
    visual) printf "sel\n" ;;
    viopp) printf "opp\n" ;;
    *) printf "???\n" ;;
  esac
}

# define right prompt, if it wasn't defined by a theme
if [[ "$RPS1" == "" && "$RPROMPT" == "" ]]; then
  # RPS1='$(vi_mode_prompt_info)'
fi
function use-j-k() {
  if [[ $((${SECONDS} - ${last:-0})) -gt 10 ]]; then
    notify-send "you can use ctrl-j and ctrl-k"
    last=${SECONDS}
  fi
}
# zle -N use-j-k
# bindkey '^[OA' use-j-k
# bindkey '^[OB' use-j-k
