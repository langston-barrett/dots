#!/usr/bin/env zsh

# Delete all files with the given extensions (list as string, separated by " ")
function remove_exts() {
  setopt shwordsplit # Split on " "
  # Group with or: Start with \(, Separate exts with \|, end with \)
  re='^.+\.\('; for ext in $1; do re+=$ext && re+='\|'; done; re+='\)$'
  # Now, find and delete based on that regexp
  find . -iregex "$re" -exec trash-put {} +
  unsetopt shwordsplit
}
function tobak() { mv "$1"{,.bak} ; }

# Description: move up this number of directories
# Arguments: $1 - number of directories to go up
# Returns (echoes): none
function upn() {
  # Description: repeat a string $1 times
  # Arguments: $1 - str, $2 - Number of times to repeat the string
  function repeat_string() {
    if [ -z "$2" ] || [ "$2" -eq 0 ];
    then echo ""
    else echo "${1}$(repeat_string "${1}" "$((${2} - 1))")"
    fi
  }
  [ -z "$1" ] && 1="1"
  cd "$(repeat_string "../" "${1}")" || echo "bad"
}

function up() {
  re='^[0-9]+$'
  if [[ ${1} =~ ${re} ]] ; then
    echo "error: enter some number of f's" >&2
  else
    upn "${#1}"
  fi
}

# https://bit.ly/2ydBgfQ
in_interactive_session() {
  [[ $- == *i* ]]
}

# https://github.com/sharkdp/fd
alias find='echo Use \\ rm or fd'

# https://github.com/sharkdp/bat
alias cat='echo Use \\ cat or bat'
alias batp='bat --paging=always'
alias less='echo Use \\ less or batp'
export BAT_PAGER='less'

alias weather='curl wttr.in/PDX'

copy_last_command() {
  printf "%s" "$history[$((HISTCMD-1))]" | xsel -ib
}
