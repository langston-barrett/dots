# Edit this configuration fible to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [];

  # List services that you want to enable:
  security.sudo.enable = true;

  users = {
    defaultUserShell = "/nix/var/nix/profiles/default/bin/zsh";
    groups = {
      siddharthist = {
        gid = 1000;
      };
    };
    users = {
      siddharthist = {
        isNormalUser = true;
        home = "/home/siddharthist";
        createHome = true;
        description = "Langston Barrett <langston dot barrett at gmail dot com>";
        uid = 1000;
        group = "siddharthist";
        extraGroups = [ "wheel" "networkmanager" ];
        shell = "/run/current-system/sw/bin/zsh";
      };
    };
  };
}
