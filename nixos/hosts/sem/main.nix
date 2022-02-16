# -*- mode: nix -*-
{ config, pkgs, ... }:

let variables = import ./variables.nix;
in {
  imports = [
    ./hardware-configuration.nix
    ./chess-openvpn.nix

    # <nixos-hardware/common/cpu/intel>

    ../../roles/laptop.nix
    ../../bluetooth.nix
    ../../steam.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    # plymouth.enable = true; # splash screen

    # Crypto!!
    initrd.luks.devices = {
      root = {
        device = "/dev/disk/by-uuid/f4d52d8a-6dd6-4182-8cba-c0dc23cfc0cb";
        preLVM = true;
        allowDiscards = true;
      };
    };
  };

  networking.hostName = "sem";
  services.logind.extraConfig = "RuntimeDirectorySize=8G";

  services.mullvad-vpn.enable = true;

  environment.systemPackages = with pkgs; [
    # beets
    # calibre
    # chromium
    # gimp
    # keynav
    # kdeconnect
    maim
    mu
    # musescore
    spotify
    tmux
    # vlc
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.05"; # Did you read the comment?
}
