# -*- mode: nix -*-
{ config, pkgs, ... }:

let master = (import ../../pkgs.nix { inherit pkgs; }).master;
    unstable = (import ../../pkgs.nix { inherit pkgs; }).unstable;
    linuxVersion =
      pkgs.lib.strings.substring 0 3
        (pkgs.lib.strings.stringAsChars
          (c: if c == "." then "_" else c)
          pkgs.linuxPackages.kernel.version);
in {
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

  # See https://github.com/NixOS/nixpkgs/pull/49703 
  nixpkgs.config = {
    config.allowUnfree = true; # dropbox, nvidia
    packageOverrides = super: let self = super.pkgs; in {
      linuxPackages = super.linuxPackages.extend (self: super: {
        nvidiaPackages = super.nvidiaPackages // {
          # 450.57.tar.gz
          stable = unstable."linuxPackages_${linuxVersion}".nvidiaPackages.beta;
          beta = unstable."linuxPackages_${linuxVersion}".nvidiaPackages.beta;
        };
      });
    };
  };

  environment.systemPackages = with pkgs; [
    glxinfo # driver query
  ];
  # services.xserver = {
  #   enable = true;
  #   videoDrivers = [ "nvidiaBeta" ];
  #   desktopManager.xfce.enable = true;
  #   # Section "Screen"
  #   #     Option         "AllowEmptyInitialConfiguration" "True"
  #   # EndSection
  #   config = ''
  #   '';
  # };

  programs.sway = {
    enable = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
