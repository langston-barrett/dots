#!/bin/bash

set -eo pipefail

# https://github.com/NixOS/nixpkgs/issues/32881

cd "$HOME/.local/share/Steam/steamapps/common/Dead Cells"
echo "588650" > steam_appid.txt # https://steamdb.info/app/588650/
export LD_PRELOAD=
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
./deadcells detect.hl
./deadcells
