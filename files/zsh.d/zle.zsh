#!/usr/bin/env zsh

# These keybindings are not necessary/as useful in the Emacs inferior shell
if [[ -z ${INSIDE_EMACS} ]] || ! [[ ${INSIDE_EMACS} =~ ".*comint.*" ]]; then
  run_porcelain() {
    porcelain "$HOME/code/+personal/porcelain/conf"
    zle -Rc
    zle reset-prompt
  }
  zle -N run_porcelain

  bindkey '^F' run_porcelain

  zle_append_to_buffer() {
    BUFFER+="${1}"
    BUFFER="${BUFFER% }"
    CURSOR=$#BUFFER
    zle redisplay
  }

  fzf-history() {
    zle -R "" "Use 'SPC i h'"
  }
  zle -N fzf-history
  bindkey '^R^R' fzf-history

  # i ----------------------------------------------------------------------------

  insert-clipboard() {
    zle_append_to_buffer "$(xsel -o)"
  }
  zle -N insert-clipboard
  bindkey -M vicmd ' ic' insert-clipboard

  fzf-insert-history() {
    zle_append_to_buffer "$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -r 's/ *[0-9]*\*? *//' | sed -r 's/\\/\\\\/g')"
  }
  zle -N fzf-insert-history
  bindkey -M vicmd ' ih' fzf-insert-history

  fzf-insert-file() {
    zle_append_to_buffer "$(fd --type f . | fzf)"
  }
  zle -N fzf-insert-file
  bindkey -M vicmd ' if' fzf-insert-file

  fzf-insert-directory() {
    zle_append_to_buffer "$(fd --type d . | fzf)"
  }
  zle -N fzf-insert-directory
  bindkey -M vicmd ' id' fzf-insert-directory

  fzf-insert-snippet() {
    zle_append_to_buffer "$(\cat ~/code/dots/files/sh.d/snippets | fzf --layout=reverse)"
  }
  zle -N fzf-insert-snippet
  bindkey -M vicmd ' is' fzf-insert-snippet

  fzf-insert-show-bindings() {
    zle -R "" "c: clipboard" "d: directory" "f: file" "h: history" "s: snippet"
  }
  zle -N fzf-insert-show-bindings
  bindkey -M vicmd ' i' fzf-insert-show-bindings

  # p ----------------------------------------------------------------------------

  find_project_root() {
    root=$(realpath ${1:-${PWD}})
    while true; do
      if [[ ${root} == "." ]]; then
        printf "%s\n" "ERROR"
        break
      fi
      if [[ ${root} == / ]] || [[ -f ${root}/.projectile ]] || [[ -d ${root}/.git  ]] || [[ -d ${root}/cabal.project  ]]; then
        break
      fi
      root=$(dirname "${root}")
    done
    printf "${root}"
  }

  pf() { cd "$(\find "$(find_project_root)" -type d | fzf --height=10% --layout=reverse --prompt='>> ')"; }
  zle -N pf
  bindkey -M vicmd ' pf' pf

  project-show-bindings() {
    zle -R "" "f: pf"
  }
  zle -N project-show-bindings
  bindkey -M vicmd ' p' project-show-bindings

  # s ----------------------------------------------------------------------------

  # TODO
  ssh-big() {
    zle -U "ssh big"
    # zle accept-and-hold
    # zle -Rc
    # zle reset-prompt
  }
  zle -N ssh-big
  bindkey -M vicmd ' sb' ssh-big

  ssh-pi() { ssh pi; }
  zle -N ssh-pi
  bindkey -M vicmd ' sp' ssh-pi

  ssh-show-bindings() {
    zle -R "" "b: big" "p: pi"
  }
  zle -N ssh-show-bindings
  bindkey -M vicmd ' s' ssh-show-bindings

  # ------------------------------------------------------------------------------

  show-bindings() {
    zle -R "" "i: insert" "s: ssh" "p: project"
  }
  zle -N show-bindings
  bindkey -M vicmd ' ' show-bindings

  # ------------------------------------------------------------------------------
  # -- bg
  # ------------------------------------------------------------------------------

  fancy-ctrl-z () {
    emulate -LR zsh
    if [[ ${#BUFFER} -eq 0 ]]; then
      bg
      zle_append_to_buffer "fg && "
    else
      zle push-input
    fi
  }
  zle -N fancy-ctrl-z
  bindkey '^Z' fancy-ctrl-z

  # ------------------------------------------------------------------------------
  # -- autofzf
  # ------------------------------------------------------------------------------

  zle_choose() {
    zle_append_to_buffer "$(printf "${1}" | fzf --height=10% --layout=reverse --prompt='>> ')"
  }

  normal_space() {
    BUFFER="${BUFFER:0:${CURSOR}} ${BUFFER:${CURSOR}}"
    CURSOR="$((CURSOR+1))"
    zle redisplay
  }

  space() {
    IFS=' ' read -A words <<< "${BUFFER}"
    # TODO: Only consider words that are before the cursor

    if [[ ${AUTOFZF} != 1 ]]; then
      normal_space
      return
    fi

    exes=$(<<'EOT'
bash
cabal
clang
echo
emacs
ghcid
git
printf
vi
which
zsh
EOT
)

    if [[ "${BUFFER}" == "" ]]; then
      zle_choose "${exes}"
      return
    fi

    normal_space

    num_words="${#words[@]}"
    if [[ "${words[1]}" == git ]]; then
      if [[ "${num_words}" == 1 ]]; then
        git_cmds=$(<<'EOT'
add
bisect
branch
checkout
clone
commit
diff
fetch
grep
init
log
merge
mv
pull
push
pushf
rebase
reset
restore
rm
show
stash
status
switch
tag
EOT
)
        zle_choose "${git_cmds}"
      fi
    fi

    # git checkout
    if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == checkout ]] && [[ "${num_words}" == 2 ]]; then
      zle_choose "$(git_list_checkout_targets)"
    elif [[ "${words[1]}" == gc ]] && [[ "${#words[@]}" == 1 ]]; then
      zle_choose "$(git_list_checkout_targets)"
    fi

    # git pull
    if [[ "${words[1]}" == git ]] && [[ "${#words[@]}" > 1 ]]; then
      if [[ "${words[2]}" == pull ]] || [[ "${words[2]}" == pl ]]; then
        if [[ "${#words[@]}" == 3 ]]; then
          zle_choose "$(git remote show)"
        elif [[ "${#words[@]}" == 4 ]]; then
          zle_choose "$(git_list_checkout_targets)"
        fi
      fi
    fi

    if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == stash ]] && [[ "${num_words}" == 2 ]]; then
      git_stash_cmds=$(<<'EOT'

apply
branch
clear
create
drop
list
pop
push
show
store
EOT
                )
      zle_choose "${git_stash_cmds}"
    fi


    if [[ "${words[1]}" == cabal ]]; then
      if [[ "${num_words}" == 1 ]]; then
        cabal_cmds=$(<<'EOT'
build
check
configure
freeze
get
haddock
info
list
repl
run
test
EOT
                )
        zle_choose "${cabal_cmds}"
      fi
    fi
  }
  zle -N space
  export AUTOFZF=1

  bindkey -M emacs " " space
  bindkey -M viins " " space
fi
