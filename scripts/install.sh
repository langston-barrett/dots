#!/usr/bin/env bash

set -ex

installed() { command -v "$1" >/dev/null 2>&1; }
bail_if_not_installed() {
  if ! installed "$1"; then
    echo "$0 requires $1, but it isn't on the PATH."
    exit 1
  fi
}

bail_if_not_installed nix

if [[ -z $XDG_CONFIG_HOME ]]; then
  export XDG_CONFIG_HOME=$HOME/.config
fi

helix_config="${XDG_CONFIG_HOME}/helix/config.toml"
if [[ -d ~/.config/helix ]] && ! [[ -f "${helix_config}" ]]; then
  curl \
    --silent \
    --show-error \
    --location \
    --output "${helix_config}" \
    https://raw.githubusercontent.com/langston-barrett/spacelix/main/src/spacelix.toml
fi

if [[ -f ~/.bashrc ]]; then
  if ! grep "BASH_CONFIG_DIR" ~/.bashrc >/dev/null 2>&1; then
    echo "Old ~/.bashrc, remove to continue"
    exit 1
  fi
else
  ln -s "$(realpath files/sh.d)" "${XDG_CONFIG_HOME}/sh.d"
  ln -s "$(realpath files/bashrc)" ~/.bashrc
  ln -s "$(realpath files/bash)" "${XDG_CONFIG_HOME}/bash"
fi

if [[ -f ~/.zshrc ]]; then
  if ! grep "ZSH_CONFIG_DIR" ~/.zshrc >/dev/null 2>&1; then
    echo "Old ~/.zshrc, remove to continue"
    exit 1
  fi
else
  ln -s "$(realpath files/zshrc)" ~/.zshrc
  ln -s "$(realpath files/zsh)" "${XDG_CONFIG_HOME}/zsh"
fi

bail_if_not_installed rustup
if ! installed cargo; then
  rustup toolchain install stable
fi

if ! installed kludge; then
  cd kludge
  cargo install --path .
fi

# TODO: rm cargo install
cargo install --path kludge
kludge install "$@"
