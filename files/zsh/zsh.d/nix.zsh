#!/usr/bin/env zsh

installed() { command -v "$1" >/dev/null 2>&1; }
if ! installed nix; then
  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
