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
  if [[ -d .git ]] || [[ -d $(find_project_root)/.git ]]; then
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
export PROMPT
unset newline
unset vi_prompt
unset git_prompt

newline() { printf '\n'; }
add-zsh-hook preexec newline

# Unrelated: bind ctrl-backspace to delete previous word
#bindkey '^H' backward-kill-word
#bindkey -r -M viins "^?"
