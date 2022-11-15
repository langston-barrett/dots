#!/usr/bin/env bash
# ^ for Shellcheck

# This is the first file to be loaded, which loads all the others

# https://kevin.burke.dev/kevin/profiling-zsh-startup-time/
if [[ "$PROFILE_ZSH_STARTUP" == true ]]; then
  # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
  PS4=$'%D{%M%S%.} %N:%i> '
  exec 3>&2 2>$HOME/tmp/startlog.$$
  setopt xtrace prompt_subst
fi

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export ZSH_CONFIG_DIR="${XDG_CONFIG_HOME}/zsh"

source "${ZSH_CONFIG_DIR}/plugins.zsh"

source_all_sh() {[[ -d $1 ]] && for f in $1/*.sh; do source "${f}"; done; unset f;}
source_all() {[[ -d $1 ]] && for f in $1/*.zsh; do source "${f}"; done; unset f;}
source_all_sh "${XDG_CONFIG_HOME}/sh.d"
source_all "${ZSH_CONFIG_DIR}/zsh.d"

if [[ "$PROFILE_ZSH_STARTUP" == true ]]; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi
