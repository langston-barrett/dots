#!/usr/bin/env bash

set -e

installed() { command -v "$1" >/dev/null 2>&1; }
bail_if_not_installed() {
  if ! installed "$1"; then
    echo "$0 requires $1, but it isn't on the PATH."
    exit 1
  fi
}


if ! [[ -f scripts/run-ansible.sh ]]; then

  bail_if_not_installed curl
  bail_if_not_installed tar

  if installed git; then
    git clone \
        --jobs "$(nproc)" \
        --shallow-submodules \
        --recurse-submodues \
        https://github.com/langston-barrett/dots
    pushd dots
    bash run.sh
  else
    dir=$PWD/dots
    if ! [[ -d $dir ]]; then
      mkdir -p "$dir"
    fi

    pushd "$dir"
    curl \
      --silent \
      --show-error \
      --location \
      --output "$PWD/dots.tar.gz" \
      "https://github.com/langston-barrett/dots/tarball/master"
    tar xvf dots.tar.gz
    pushd ./langston-barrett-dots*/
  fi
  bash run.sh
  popd

else
  if ! installed nix; then

    bail_if_not_installed curl
    bail_if_not_installed sudo

    mkdir -m 0755 /nix && chown root /nix
    curl --silent --show-error https://nixos.org/nix/install | sh
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
  fi

  # sudo nix-channel --update
  nix-channel --update

  if ! (nix-channel --list | grep "home-manager" >/dev/null 2>&1); then
    nix-channel --add https://github.com/rycee/home-manager/archive/release-21.05.tar.gz home-manager
    nix-channel --update
  fi

  if [[ -n $XDG_CONFIG_HOME ]]; then
    export XDG_CONFIG_HOME=$HOME/.config
  fi
  # nix-shell default.nix  \
  #           --substituters "https://cache.nixos.org" \
  #           --run \
  #           'bash scripts/shellcheck.sh'
  nix-shell default.nix  \
            --substituters "https://cache.nixos.org" \
            --run \
            "bash scripts/run-ansible.sh"
fi

if [[ -f ~/.bashrc ]]; then
  if ! grep "bash.d" ~/.bashrc >/dev/null 2>&1; then
    echo "Old ~/.bashrc, remove to continue"
    exit 1
  fi
else
  ln -s "$(realpath files/bashrc)" ~/.bashrc
fi

if [[ -f ~/.zshrc ]]; then
  if ! grep "zsh.d" ~/.zshrc >/dev/null 2>&1; then
    echo "Old ~/.zshrc, remove to continue"
    exit 1
  fi
else
  ln -s "$(realpath files/zshrc)" ~/.zshrc
fi
