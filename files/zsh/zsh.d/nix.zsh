#!/usr/bin/env zsh

installed() { command -v "$1" >/dev/null 2>&1; }
if ! installed nix; then
  # shellcheck disable=SC1091
  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
