#!/usr/bin/env bash

set -ex

if [[ -f scripts/install.sh ]]; then
  exec bash scripts/install.sh
fi

installed() { command -v "$1" >/dev/null 2>&1; }
bail_if_not_installed() {
  if ! installed "$1"; then
    echo "$0 requires $1, but it isn't on the PATH."
    exit 1
  fi
}

if installed git; then
  git clone \
      --jobs "$(nproc)" \
      https://github.com/langston-barrett/dots
  cd dots
else

  bail_if_not_installed curl
  bail_if_not_installed tar

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

exec bash run.sh

