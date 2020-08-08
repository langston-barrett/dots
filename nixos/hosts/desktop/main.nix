# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration-desktop.nix
    ../common.nix
    ../general.nix

    # ../audio.nix
    ../networking.nix
    # ../steam.nix
    ../x.nix
    # ../wayland.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    # plymouth.enable = true; # splash screen
  };

  networking.hostName = "langston-desktop"; # Define your hostname.
  # networking.networkmanager.enable = true;

  nixpkgs.config.allowUnfree = true; # dropbox

  environment.systemPackages = with pkgs; [
  ];
  services.openssh = {
    enable = true;
    forwardX11 = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
