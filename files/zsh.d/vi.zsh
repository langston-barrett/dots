#!/usr/bin/env zsh

# http://stratus3d.com/blog/2017/10/26/better-vi-mode-in-zshell/

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
# export KEYTIMEOUT=20

# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/vi-mode/vi-mode.plugin.zsh

# Updates editor information when the keymap changes.
function zle-keymap-select() {
  # update keymap variable for the prompt
  VI_KEYMAP=$KEYMAP

  zle reset-prompt
  zle -R
}

zle -N zle-keymap-select

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
  printf "${${VI_KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}\n"
}

# define right prompt, if it wasn't defined by a theme
if [[ "$RPS1" == "" && "$RPROMPT" == "" ]]; then
  # RPS1='$(vi_mode_prompt_info)'
fi

# allow ctrl-r and ctrl-s to search the history
bindkey '^r' history-incremental-search-backward
bindkey '^s' history-incremental-search-forward

# allow ctrl-a and ctrl-e to move to beginning/end of line
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line

# allow ctrl-a and ctrl-e to move to beginning/end of line
#
# bizarrely, these interfere with emacs + vterm
# https://github.com/hlissner/doom-emacs/issues/3546
if [[ -z ${INSIDE_EMACS} ]]; then
  bindkey '^k' up-line-or-history
  bindkey '^j' down-line-or-history
fi
function use-j-k() {
  if [[ $((${SECONDS} - ${last:-0})) -gt 10 ]]; then
    notify-send "you can use ctrl-j and ctrl-k"
    last=${SECONDS}
  fi
}
zle -N use-j-k
bindkey '^[OA' use-j-k
bindkey '^[OB' use-j-k
