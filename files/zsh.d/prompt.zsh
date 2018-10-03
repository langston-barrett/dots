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
local final_char=' %{$reset_color%}'
local time='(%D{%K:%M}) %{$reset_color%}'
local ns='$(in_nix_shell)'

PROMPT="${final_char}"
RPROMPT=""
