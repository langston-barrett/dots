# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/common/cpu/intel>

    # Include the results of the hardware scan.
    ./hardware-configuration-x1.nix
    ../common.nix
    ../general.nix

    ../audio.nix
    ../networking.nix
    ../steam.nix
    ../x.nix
    # ../wayland.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    # plymouth.enable = true; # splash screen

    # Crypto!!
    initrd.luks.devices = {
      root = {
        device = "/dev/disk/by-uuid/9fc9c5f4-6ad6-4bce-bd55-1cbe26e42e02";
        preLVM = true;
        allowDiscards = true;
      };
    };
  };

  networking.hostName = "langston-x1"; # Define your hostname.
  # networking.networkmanager.enable = true;

  services.xserver.synaptics = {
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

  nixpkgs.config.allowUnfree = true; # dropbox

  programs.light.enable = true;
  # programs.java.enable = true;

  environment.systemPackages = with pkgs; [
    beets
    calibre
    # chromium
    comfortaa
    # gimp
    # keynav
    kdeconnect
    maim
    mu
    # musescore
    oxygenfonts
    redshift
    spotify
    tmux
    vlc
    ympd

    # python development
    # python
    # pythonPackages.importmagic
  ];

  # Can't be enabled in virtual guests
  #rngd.enable = true; # feed hardware randomness to kernel when possible

  # Printing
  # services.printing = {
  #   enable  = true;
  #   drivers = [ pkgs.hplip ];
  # };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
