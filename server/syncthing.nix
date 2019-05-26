{ pkgs ? import <nixpkgs> { } }:

import ./base.nix {
  name = "syncthing";
  opts = [ "-gui-address=0.0.0.0:8384" ];
}
