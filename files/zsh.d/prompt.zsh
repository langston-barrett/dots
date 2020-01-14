autoload -U promptinit && promptinit
autoload -U colors && colors
setopt promptsubst

function in_nix_shell() {
  if [[ ! -z $IN_NIX_SHELL ]]; then
    echo "(nxsh: $IN_NIX_SHELL)"
  else
    echo "(nxsh: no)"
  fi
}

# The convention is a single space at the end
local dir_2='%{$fg_bold[0]%}%2~% %{$reset_color%}'
local final_char='> %{$reset_color%}'
local time='(%D{%K:%M}) %{$reset_color%}'
local ns='$(in_nix_shell)'

# If we're in tmux, the directory is shown on the status line
if [[ -z $TMUX ]]; then
  PROMPT="${dir_2} ${final_char}"
else
  PROMPT="${final_char}"
fi

# If we're on another machine, show the user/hostname
if [[ $(uname -n) != langston-nixos ]]; then
  PROMPT="$(whoami)@$(uname -n) ${dir_2} > "
fi

# Unrelated: bind ctrl-backspace to delete previous word
bindkey '^H' backward-kill-word

# https://github.com/akermu/emacs-libvterm#directory-tracking
function vterm_printf() {
  if [ -n "$TMUX" ]; then
    # tell tmux to pass the escape sequences through
    # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}
vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
