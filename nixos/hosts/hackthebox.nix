# -*- mode: nix -*-
{ config, pkgs, ... }:

let unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) { };
in {
  imports = [
    ../common.nix
    ../x.nix
  ];
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # python3
    unstable.metasploit
    unstable.nmap
    openvpn
    netcat-openbsd
  ];
  system.stateVersion = "18.03";
}
