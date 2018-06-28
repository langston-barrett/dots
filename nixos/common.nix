# This files imports any others that are completely portable, i.e.
# should be used on any and every machine. It should be imported in each
# ./hosts/*.nix

{ config, pkgs, ... }:

{
  imports = [
    ./networking.nix
    ./security.nix
    ./users.nix
    ./zsh.nix
  ];


  i18n = {
    # Use a bigger font for HiDPI displays
    consoleFont = "sun12x22";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  services.emacs.enable = true;
  nixpkgs.config.allowUnfree = true;

  # Time
  #services.chrony.enable = true;
  time.timeZone = "America/Los_Angeles";
  services.localtime.enable = true;
  # services.tzupdate.enable = true;

  nix = {
    maxJobs = 4;
    buildCores = 4;
    gc.automatic = true;
    useSandbox = true;
    # Use a local clone of nixpkgs at /etc/nixpkgs
    #nixPath = [ "nixos-config=/etc/nixos/configuration.nix" "/home/siddharthist/code" ];
  };
  system.autoUpgrade.enable = true;

  environment.systemPackages = with pkgs; [
    aspell # TODO: english dict?
    atool # "compress" command in ranger
    curl
    emacs
    exfat
    file
    git
    gnumake
    gnupg
    imagemagick
    mpw
    pass
    p7zip
    ranger
    silver-searcher # ag
    sudo
    tldr
    unzip
    xclip # for pass
    xsel
    zip
  ];

  # Other common ones:
  # gcc #
  # python
  # openssl
  # # Try: nix-env -qaA nixos.haskellPackages
  # (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
  #   cabal-install
  #   hoogle
  #   HUnit
  #   QuickCheck
  #   tasty
  #   tasty-hunit
  #   tasty-quickcheck
  #   text
  # ])) #
}
