# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    <nixos-hardware/common/cpu/intel>

    ./x1-openvpn.nix

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
        device = "/dev/disk/by-uuid/9fc9c5f4-6ad6-4bce-bd55-1cbe26e42e02";
        preLVM = true;
        allowDiscards = true;
      };
    };
  };

  networking.hostName = "langston-x1"; # Define your hostname.

  environment.systemPackages = with pkgs; [
    beets
    # calibre
    # chromium
    comfortaa
    # gimp
    # keynav
    # kdeconnect
    maim
    mu
    # musescore
    oxygenfonts
    redshift
    spotify
    tmux
    vlc
    # ympd

    # python development
    # python
    # pythonPackages.importmagic
  ];

  # Just testing
  services.postgresql = {
    enable = false;
    ensureUsers = [
      {
        name = "superuser";
        ensurePermissions = {
          "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        };
      }
    ];
  };



  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
