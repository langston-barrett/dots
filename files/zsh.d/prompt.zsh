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
