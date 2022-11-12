autoload -U promptinit && promptinit
autoload -U colors && colors
setopt promptsubst

function in_nix_shell() {
  if [[ ! -z ${IN_NIX_SHELL} ]]; then
    echo "(nxsh: $IN_NIX_SHELL)"
  else
    echo "(nxsh: no)"
  fi
}

PROMPT_SEP=" : "

git_prompt_info() {
  if [[ -d .git ]] || [[ -d ${PROJECT_ROOT}/.git ]]; then
    ref=$(git symbolic-ref --short HEAD 2> /dev/null)
    if [[ -n "${ref}" ]]; then
      printf "%s%s" "${PROMPT_SEP}" "${ref}"
    fi
  fi
}

# If we're on another machine, show the user/hostname
if [[ $(uname -n) != big ]] && [[ $(uname -n) != langston-x1 ]]; then
  PROMPT_EXTRA="${PROMPT_EXTRA}$(whoami)@$(uname -n)${PROMPT_SEP}"
fi

newline=$'\n'
vi_prompt='$(vi_mode_prompt_info)'
git_prompt='$(git_prompt_info)'
PROMPT="${newline}[${vi_prompt}${PROMPT_SEP}${PROMPT_EXTRA}%3d${git_prompt}]${newline}${newline}"
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
