autoload -U promptinit && promptinit
autoload -U colors && colors
setopt promptsubst

# NB: Spacing is a Unicode em-space
PROMPT_SEP=" : "

# Update prompt every second, except when completing
TMOUT=1

TRAPALRM() {
  # if [[ $((SECONDS % 2)) -eq 0 ]]; then
  #   printf "${SECONDS}" > /tmp/now
  # fi
  return
  # TODO: Also don't do this during history search
  if [ "$WIDGET" != "complete-word" ] && [ "$WIDGET" != "tab" ]; then
    zle reset-prompt
  fi
}

# Include auto-updating build status in prompt
# function watch() {
#   if [[ -f .nowatch ]]; then
#     return
#   fi
#   if [[ zbr == $(basename "${PWD}") ]] && [[ -f Cargo.toml ]]; then
#     if [[ -f /tmp/watchpid ]]; then
#       kill "$(bat --plain /tmp/watchpid)"
#     fi
#     status_file="/tmp/$(basename "${PWD}")-status"
#     (ls /tmp/now ./**/*.toml ./**/*.rs | entr -c -s check.sh > "${status_file}.log" 2>&1) & disown
#     printf "%s" "$?" > /tmp/watchpid
#   fi
# }
# chpwd_functions=(${chpwd_functions[@]} "watch")

# function build_info() {
#   status_file="/tmp/$(basename "${PROJECT_ROOT}")-status"
#   if [[ -f ${status_file} ]]; then
#     printf "%s%s" "${PROMPT_SEP}" "$(<"${status_file}")"
#   fi
# }

# If we're on another machine, show the user/hostname
if [[ $(uname -n) != big ]] && [[ $(uname -n) != langston-x1 ]]; then
  PROMPT_EXTRA="${PROMPT_EXTRA}$(whoami)@$(uname -n)${PROMPT_SEP}"
fi

newline=$'\n'
kludge_prompt='$(kludge prompt)'
PROMPT="${newline}[${PROMPT_EXTRA}%3d${kludge_prompt}]${newline}${newline}"
#PROMPT="$ "
export PROMPT
unset newline

newline() {
  printf '\n'
}
add-zsh-hook preexec newline

# https://superuser.com/questions/1389834/can-i-have-the-terminal-prompt-at-the-vertical-middle-of-a-window
zmodload zsh/terminfo 
PS1o="$PS1"
function prompt_middle() { 
  halfpage_down=""
  for i in {1..${1}}; do
    halfpage_down="$halfpage_down${terminfo[cud1]}"
  done
  halfpage_up=""
  for i in {1..${1}}; do
    halfpage_up="$halfpage_up${terminfo[cuu1]}"
  done
  PS1="%{${halfpage_down}${halfpage_up}%}$PS1o"; 
}
function prompt_float() { PS1="$PS1o"; }
function prompt_height() {
  if [[ ${LINES} -gt 60 ]]; then
    prompt_middle $((LINES/2))
  else
    prompt_float
  fi
}
prompt_height

# Reset when changing window size. Currently leads to a bit of weirdness, but
# nothing major.
#
# https://unix.stackexchange.com/questions/360600/reload-zsh-when-resizing-terminator-window
TRAPWINCH() {
  prompt_height
  zle -R
}

# https://github.com/romkatv/powerlevel10k/issues/563
printf '\n%.0s' {1..100}

# Unrelated: bind ctrl-backspace to delete previous word
#bindkey '^H' backward-kill-word
#bindkey -r -M viins "^?"
