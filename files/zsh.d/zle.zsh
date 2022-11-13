#!/usr/bin/env bash
# The above is for Shellcheck

zlefzf() {
  local start_col=0
  if [[ "${#BUFFER}" -lt $((COLUMNS-32)) ]]; then
    start_col="${#BUFFER}"
  fi

  fzf --margin=0,0,0,"${start_col}" \
      --height=10% \
      --layout=reverse \
      --prompt='' \
      --info=hidden \
      --bind="bspace:backward-delete-char/eof" \
      --bind="tab:replace-query" \
      "${@}"
}

zlefzff() {
  zlefzf --preview 'preview.sh {}' "${@}"
}

zle_append_to_buffer() {
  BUFFER+="${1}"
  CURSOR="${#BUFFER}"
  zle redisplay
}

zle_write_buffer() {
  BUFFER="${1}"
  CURSOR="${#BUFFER}"
  zle redisplay
}

# zle_bind() {
#   zle -N "${1}"
#   bindkey -M vicmd " ${2}" "${1}"
# }

# Create a new keymap for spacezle. In vi command mode, space enters the new
# keymap. ESC exits it.
bindkey -N spacezle vicmd
bindkey -M spacezle '^[' vi-cmd-mode
spacezle-map() {
  zle -K spacezle
  show-bindings
}
zle -N spacezle-map
bindkey -M vicmd ' ' spacezle-map

zle_bind() {
  zle -N "${1}"
  bindkey -M spacezle "${2}" "${1}"
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
  echo "c: compile"
  echo "k: check"
  echo "l: lint"
  echo "t: test"
}

compile-show-bindings() {
  zle -M "$(compile-print-bindings)"
}
zle -N compile-show-bindings
bindkey -M vicmd ' c' compile-show-bindings

# d ----------------------------------------------------------------------------

dir-cd() {
  cd "$(\find "${PWD}" -type d | zlefzf)" || true
}
zle_bind dir-cd 'dd'

dir-pwd() {
  zle_append_to_buffer "$(pwd)"
}
zle_bind dir-pwd 'dw'

dir-print-bindings() {
  echo "d: cd"
  echo "w: pwd"
}

dir-show-bindings() {
  zle -M "$(dir-print-bindings)"
}
zle_bind dir-show-bindings 'd'

# q ----------------------------------------------------------------------------

quit-quit() {
  zle_append_to_buffer "exit"
  zle vi-accept-line
}
zle_bind quit-quit 'qq'

quit-reload() {
  zle_append_to_buffer "exec zsh"
  zle vi-accept-line
}
zle_bind quit-reload 'qr'

quit-print-bindings() {
  echo "q: quit"
  echo "r: reload"
}

quit-show-bindings() {
  zle -M "$(quit-print-bindings)"
}
zle_bind quit-show-bindings 'q'

# i ----------------------------------------------------------------------------

insert-clipboard() {
  zle_append_to_buffer "$(xsel -ob)"
}
zle_bind insert-clipboard 'ic'

fzf-insert-directory() {
  zle_append_to_buffer "$(fd --type d . | zlefzf)"
}
zle_bind fzf-insert-directory 'id'

fzf-insert-exact-history() {
  zle_append_to_buffer "$(list-history | zlefzf --tac)"
}
zle_bind fzf-insert-exact-history 'ie'

fzf-insert-file() {
  zle_append_to_buffer "$(fd --type f . | zlefzf)"
}
zle_bind fzf-insert-file 'if'

fzf-insert-history() {
  zle_append_to_buffer "$(list-history | zlefzf --tac)"
}
zle_bind fzf-insert-history 'ih'

fzf-insert-project() {
  zle_append_to_buffer "$(fd "${PROJECT_ROOT}" | zlefzf)"
}
zle_bind fzf-insert-project 'ip'

fzf-insert-snippet() {
  zle_append_to_buffer "$(zlefzf < ~/code/dots/files/sh.d/snippets)"
}
zle_bind fzf-insert-snippet 'is'

insert-print-bindings() {
  echo "c: clipboard"
  echo "d: directory"
  echo "f: file"
  echo "h: history"
  echo "p: project"
  echo "s: snippet"
}

insert-show-bindings() {
  zle -M "$(insert-print-bindings)"
  # bindkey -M vicmd 'c' insert-clipboard
  # bindkey -M vicmd 'd' fzf-insert-directory
  # bindkey -M vicmd 'f' fzf-insert-file
  # bindkey -M vicmd 'h' fzf-insert-history
  # bindkey -M vicmd 'p' fzf-insert-project
  # bindkey -M vicmd 's' fzf-insert-snippet
}
zle_bind insert-show-bindings 'i'

# l ----------------------------------------------------------------------------

zle-clear() {
  clear
  zle redisplay
}
zle_bind zle-clear 'l'

# p ----------------------------------------------------------------------------

project-cd() {
  cd "$(fd --type d "${PROJECT_ROOT}" | fzf --height=10% --layout=reverse --prompt='')" || true
  zle redisplay
}
zle_bind project-cd 'pc'

project-editor() {
  cd "${PROJECT_ROOT}" || truereturn
  ee
}
zle_bind project-editor 'pe'

project-file() {
  cd "${PROJECT_ROOT}" || return
  ee "$(fd --type f . | fzf --height=10% --layout=reverse --prompt='')"
}
zle_bind project-file 'pf'

project-print-bindings() {
  echo "c: cd"
  echo "e: editor"
  echo "f: file"
}

project-show-bindings() {
  zle -M "$(project-print-bindings)"
}
zle_bind project-show-bindings 'p'

# s ----------------------------------------------------------------------------

# ssh

# T ----------------------------------------------------------------------------

bindkey -M vicmd ' Ta' autosuggest-toggle

# y ----------------------------------------------------------------------------

yank-cwd() {
  pwd | xsel -ib
  zle -R "" "copied '$()'"
}
zle_bind yank-cwd 'yc'


yank-last() {
  copy_last_command
  zle -R "" "copied '$(xsel -ob | head -n 1)'"
}
zle_bind yank-last 'yl'

yank-rerun() {
  zle_append_to_buffer "$history[$((HISTCMD-1))] 2>&1 |& xsel -ib"
}
zle_bind yank-rerun 'yr'

yank-print-bindings() {
  echo "c: cwd"
  echo "l: last"
  echo "r: re-run"
}

yank-show-bindings() {
  zle -M "$(yank-print-bindings)"
}
zle_bind yank-show-bindings 'y'

# z ----------------------------------------------------------------------------

zle-z() {
  zle_append_to_buffer "z "
  zle vi-insert
}
zle -N zle-z
bindkey -M vicmd 'z' zle-z

# ------------------------------------------------------------------------------

autoload -U edit-command-line
zle_bind edit-command-line 'e'

print-bindings() {
  echo "c: compile"
  echo "d: dir"
  echo "e: edit"
  echo "i: insert"
  echo "s: ssh"
  echo "p: project"
  echo "y: yank"
  echo "z: z"
}

show-bindings() {
  zle -M "$(print-bindings)"
}
# zle_bind show-bindings ''

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
