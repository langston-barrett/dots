{ config, pkgs, ... }:

{
  imports = [
      ./audio.nix
      ./hardware-configuration.nix
      ./networking.nix
      ./packages.nix
      ./ssh.nix
      ./steam.nix
      ./systemd.nix
      ./users.nix
      ./x.nix
      ./zsh.nix
  ];

  # Use the gummiboot efi boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  i18n = {
    # Use a bigger font for HiDPI displays
    consoleFont = "sun12x22";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "USA/Los_Angeles";

  nixpkgs.config.allowUnfree = true;

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      fira-code
      # noto-fonts-emoji-git
      opensans-ttf
      # TODO: get Monoid in nixpkgs
      # TODO: update this package in nixpkgs
      # symbola
    ];
  };

  hardware.bluetooth.enable = true;
  services.chrony.enable = true;

  nix = {
    maxJobs = 4;
    buildCores = 4;
    gc.automatic = true;
    useSandbox = true;
    # Use a local clone of nixpkgs at /etc/nixpkgs
    #nixPath = [ "nixos-config=/etc/nixos/configuration.nix" "/home/siddharthist/code" ];
  };
}
