# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../roles/laptop.nix
    ../../steam.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Crypto!!
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/445eed41-be61-44fa-9cd0-ffea26dea921";
      preLVM = true;
      allowDiscards = true;
    };
  };

  networking.hostName = "langston-nixos";

  environment.systemPackages = [
    pkgs.calibre
    pkgs.maim
    pkgs.mu
    pkgs.spotify
    pkgs.tmux
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
