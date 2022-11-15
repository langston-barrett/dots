#!/usr/bin/env zsh

# See NixOS/nix#1056
if [ -n "$IN_NIX_SHELL" ]; then
  export TERMINFO=/run/current-system/sw/share/terminfo

  # Reload terminfo
  real_TERM=$TERM; TERM=xterm; TERM=$real_TERM; unset real_TERM
fi
