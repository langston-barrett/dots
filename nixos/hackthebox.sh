#!/usr/bin/env bash

set -e

user=siddharthist
# sudo passwd siddharthist pass
# sudo passwd vagrant pass

# TODO: debug this
if [[ ! -d /home/${user}/.config ]]; then
  rm -rf dots/ || true
  git clone --branch master --depth 1 https://github.com/langston-barrett/dots
  cd dots/
  HOME=/home/${user} bash run.sh || true
fi

sudo chown -R ${user}:${user} /home/${user}
# TODO: delete me
sudo nix-env -i openvpn
sudo openvpn /etc/nixos/host/*.ovpn & disown
