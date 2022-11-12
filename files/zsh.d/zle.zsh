#!/usr/bin/env bash
# The above is for Shellcheck

zlefzf() {
   fzf --margin=0,0,0,"${#BUFFER}" \
       --height=10% \
       --layout=reverse \
       --prompt='' \
       --info=hidden \
       --bind="bspace:backward-delete-char/eof" \
       --bind="tab:replace-query" \
       "${@}"
}

zle_append_to_buffer() {
  BUFFER+="${1}"
  CURSOR=$#BUFFER
  zle redisplay
}

zle_write_buffer() {
  BUFFER="${1}"
  CURSOR=$#BUFFER
  zle redisplay
}

# c ----------------------------------------------------------------------------

count_files_with_ext() { ls -1 ./*."${1}" 2>/dev/null | wc -l; }

cabal_compile_cmd() {
  # TODO(lb): fzf target
  printf "cabal build"
}

make_compile_cmd() {
  # TODO(lb): fzf target
  printf "cabal build"
}

compile_cmd() {
  if [[ -f Makefile ]]; then
    make_compile_cmd
  elif [[ $(count_files_with_ext cabal) != 0 ]]; then
    cabal_compile_cmd
  fi
}

chk() {
  # TODO(lb): cd to project root?
  zle_write_buffer "$(lint_cmd) && $(compile_cmd) && $(test_cmd)"
}
zle -N chk
bindkey -M vicmd ' ck' chk

compile() {
  # TODO(lb): cd to project root?
  zle_write_buffer "$(compile_cmd)"
}
zle -N compile
bindkey -M vicmd ' cc' compile

cabal_lint_cmd() {
  printf "hlint src test"
}

make_lint_cmd() {
  # TODO(lb): fzf target
  printf "make lint"
}

fd_src_cmd() {
  if [[ -f Makefile ]]; then
    printf "fd --extension c --extension h ."
  elif [[ $(count_files_with_ext cabal) != 0 ]]; then
    printf "fd --extension hs ."
  fi
}

run_entr() {
  # TODO(lb): cd to project root?
  zle_write_buffer "$(fd_src_cmd) | entr -c -s \"$(lint_cmd) && $(compile_cmd)\""
}
zle -N run_entr
bindkey -M vicmd ' ce' run_entr

lint_cmd() {
  if [[ -f Makefile ]]; then
    make_lint_cmd
  elif [[ $(count_files_with_ext cabal) != 0 ]]; then
    cabal_lint_cmd
  fi
}

lint() {
  # TODO(lb): cd to project root?
  zle_write_buffer "$(lint_cmd)"
}
zle -N lint
bindkey -M vicmd ' cl' lint

cabal_test_cmd() {
  printf "cabal test"
}

make_test_cmd() {
  # TODO(lb): fzf target
  printf "make test"
}

test_cmd() {
  if [[ -f Makefile ]]; then
    make_test_cmd
  elif [[ $(count_files_with_ext cabal) != 0 ]]; then
    cabal_test_cmd
  fi
}

tests() {
  # TODO(lb): cd to project root?
  zle_write_buffer "$(test_cmd)"
}
zle -N tests
bindkey -M vicmd ' ct' tests

compile-print-bindings() {
  echo "c: compile" "k: check" "l: lint" "t: test"
}

compile-show-bindings() {
  zle -R "" "$(compile-print-bindings)"
}
zle -N compile-show-bindings
bindkey -M vicmd ' c' compile-show-bindings

# d ----------------------------------------------------------------------------

dir-cd() {
  bindkey -rM vicmd 'd'
  cd "$(\find $(pwd) -type d | zlefzf)"
}
zle -N dir-cd
bindkey -M vicmd ' dd' dir-cd

dir-pwd() {
  bindkey -rM vicmd 'w'
  zle_append_to_buffer "$(pwd)"
}
zle -N dir-pwd
bindkey -M vicmd ' dw' dir-pwd

dir-print-bindings() {
  echo "d: cd" "w: pwd"
}

dir-show-bindings() {
  bindkey -rM vicmd 'd'
  zle -R "" "$(dir-print-bindings)"
  bindkey -M vicmd 'd' dir-cd
  bindkey -M vicmd 'w' dir-pwd
}
zle -N dir-show-bindings
bindkey -M vicmd ' d' dir-show-bindings

# i ----------------------------------------------------------------------------

insert-clipboard() {
  bindkey -rM vicmd 'c'
  zle_append_to_buffer "$(xsel -ob)"
}
zle -N insert-clipboard
bindkey -M vicmd ' ic' insert-clipboard

fzf-insert-directory() {
  bindkey -rM vicmd 'd'
  zle_append_to_buffer "$(fd --type d . | zlefzf)"
}
zle -N fzf-insert-directory
bindkey -M vicmd ' id' fzf-insert-directory

fzf-insert-exact-history() {
  bindkey -rM vicmd 'e'
  zle_append_to_buffer "$(list-history | zlefzf --tac)"
}
zle -N fzf-insert-exact-history
bindkey -M vicmd ' ie' fzf-insert-exact-history

fzf-insert-file() {
  bindkey -rM vicmd 'f'
  zle_append_to_buffer "$(fd --type f . | zlefzf)"
}
zle -N fzf-insert-file
bindkey -M vicmd ' if' fzf-insert-file

fzf-insert-history() {
  bindkey -rM vicmd 'h'
  zle_append_to_buffer "$(list-history | zlefzf --tac)"
}
zle -N fzf-insert-history
bindkey -M vicmd ' ih' fzf-insert-history

fzf-insert-project() {
  bindkey -rM vicmd 'p'
  zle_append_to_buffer "$(fd "${PROJECT_ROOT}" | zlefzf)"
}
zle -N fzf-insert-project
bindkey -M vicmd ' ip' fzf-insert-project

fzf-insert-snippet() {
  bindkey -rM vicmd 's'
  zle_append_to_buffer "$(zlefzf < ~/code/dots/files/sh.d/snippets)"
}
zle -N fzf-insert-snippet
bindkey -M vicmd ' is' fzf-insert-snippet

insert-print-bindings() {
  echo "c: clipboard" "d: directory" "f: file" "h: history" "p: project" "s: snippet"
}

insert-show-bindings() {
  bindkey -rM vicmd 'i'
  bindkey -M vicmd 'i' vi-insert
  zle -R "" "$(insert-print-bindings)"
  # bindkey -M vicmd 'c' insert-clipboard
  # bindkey -M vicmd 'd' fzf-insert-directory
  # bindkey -M vicmd 'f' fzf-insert-file
  # bindkey -M vicmd 'h' fzf-insert-history
  # bindkey -M vicmd 'p' fzf-insert-project
  # bindkey -M vicmd 's' fzf-insert-snippet
}
zle -N insert-show-bindings
bindkey -M vicmd ' i' insert-show-bindings

# l ----------------------------------------------------------------------------

zle-clear() {
  clear
  zle redisplay
}
zle -N zle-clear
bindkey -M vicmd ' l' zle-clear

# p ----------------------------------------------------------------------------

project-cd() {
  bindkey -rM vicmd 'c'
  cd "$(fd --type d "${PROJECT_ROOT}" | fzf --height=10% --layout=reverse --prompt='')"
  zle redisplay
}
zle -N project-cd
bindkey -M vicmd ' pc' project-cd

project-editor() {
  bindkey -rM vicmd 'e'
  cd "${PROJECT_ROOT}"
  ee
}
zle -N project-editor
bindkey -M vicmd ' pe' project-editor

project-file() {
  bindkey -rM vicmd 'f'
  cd "${PROJECT_ROOT}"
  ee "$(fd --type f . | fzf --height=10% --layout=reverse --prompt='')"
}
zle -N project-file
bindkey -M vicmd ' pf' project-file

project-print-bindings() {
  echo "c: cd" "e: editor" "f: file"
}

project-show-bindings() {
  bindkey -rM vicmd 'p'
  zle -R "" "$(project-print-bindings)"
  bindkey -M vicmd 'c' project-cd
  bindkey -M vicmd 'e' project-editor
  bindkey -M vicmd 'f' project-file
}
zle -N project-show-bindings
bindkey -M vicmd ' p' project-show-bindings

# s ----------------------------------------------------------------------------

# ssh

# T ----------------------------------------------------------------------------

bindkey -M vicmd ' Ta' autosuggest-toggle

# y ----------------------------------------------------------------------------

yank-cwd() {
  bindkey -rM vicmd 'c'
  pwd | xsel -ib
  zle -R "" "copied '$()'"
}
zle -N yank-cwd
bindkey -M vicmd ' yc' yank-cwd


yank-last() {
  bindkey -rM vicmd 'l'
  copy_last_command
  zle -R "" "copied '$(xsel -ob | head -n 1)'"
}
zle -N yank-last
bindkey -M vicmd ' yl' yank-last

yank-rerun() {
  bindkey -rM vicmd 'r'
  zle_append_to_buffer "$history[$((HISTCMD-1))] 2>&1 |& xsel -ib"
}
zle -N yank-rerun
bindkey -M vicmd ' yr' yank-rerun

yank-print-bindings() {
  echo "c: cwd" "l: last" "r: re-run"
}

yank-show-bindings() {
  bindkey -rM vicmd 'y'
  zle -R "" "$(yank-print-bindings)"
  bindkey -M vicmd 'c' yank-cwd
  bindkey -M vicmd 'l' yank-last
  bindkey -M vicmd 'r' yank-rerun
}
zle -N yank-show-bindings
bindkey -M vicmd ' y' yank-show-bindings

# ------------------------------------------------------------------------------

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd ' e' edit-command-line

show-bindings() {
  zle -R "" "c: compile" "d: dir" "e: edit" "i: insert" "s: ssh" "p: project" "y: yank"
  bindkey -M vicmd 'c' compile-show-bindings
  bindkey -M vicmd 'd' dir-show-bindings
  bindkey -M vicmd 'i' insert-show-bindings
  bindkey -M vicmd 'p' project-show-bindings
  bindkey -M vicmd 'y' yank-show-bindings
}
zle -N show-bindings
bindkey -M vicmd ' ' show-bindings

# TODO
help() {
  show-bindings
  printf "[c] "
  compile-print-bindings
  printf "[i] "
  insert-print-bindings
}

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
