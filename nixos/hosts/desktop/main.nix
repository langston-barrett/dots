# -*- mode: nix -*-
{ config, pkgs, ... }:

{
 imports = [
    ./hardware-configuration.nix

    ../../roles/server.nix

    ../../audio.nix
    ../../networking.nix
    ../../steam.nix
    ../../x.nix
    # ../wayland.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    # plymouth.enable = true; # splash screen
  };

  networking.hostName = "langston-desktop"; # Define your hostname.

  environment.systemPackages = with pkgs; [
    glxinfo # driver query
  ];
  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    desktopManager.xfce.enable = true;
  };

  nix = {
    buildCores = 0;
    maxJobs = 24;
    # gc.automatic = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
