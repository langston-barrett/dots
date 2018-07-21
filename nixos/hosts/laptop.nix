# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration-laptop.nix
    ../common.nix

    ../audio.nix
    ../mail.nix
    ../x.nix
    # ../wayland.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Crypto!!
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/445eed41-be61-44fa-9cd0-ffea26dea921";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  networking.hostName = "langston-nixos"; # Define your hostname.
  # networking.networkmanager.enable = true;

  services.xserver = {
    synaptics = {
      enable = true;
      # OSX-like "Natural" two-finger scrolling
      twoFingerScroll = true;
      horizTwoFingerScroll = true;
      horizEdgeScroll = false;
      additionalOptions = ''
        Option "VertScrollDelta"  "-75"
        Option "HorizScrollDelta" "-75"
      '';
    };
  };

  programs.light.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  environment.systemPackages = with pkgs; [
    anki
    beets
    calibre
    chromium
    comfortaa
    dropbox
    gimp
    htop
    kdeconnect
    lxappearance
    maim
    mu
    oxygenfonts
    redshift
    spotify
    tmux
    transgui
    transmission
    vlc
    ympd
    zotero
  ];

  # virtualisation.virtualbox.enableHardening = true;
  virtualisation.virtualbox.host.enable = true;
  services.physlock.enable = true;

  # Can't be enabled in virtual guests
  #rngd.enable = true; # feed hardware randomness to kernel when possible

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
