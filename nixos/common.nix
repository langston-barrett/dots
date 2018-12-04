# This files imports any others that are completely portable, i.e.
# should be used on any and every machine. It should be imported in each
# ./hosts/*.nix

{ config, pkgs, ... }:

{
  imports = [
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

    # Use cachix binary caches
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];
    trustedUsers = [ "root" "siddharthist" ];

  };
  system.autoUpgrade.enable = true;

  xdg = {
    icons.enable = true;
    menus.enable = true;
    mime.enable  = true;
  };

  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: with ds; [ en ]))
    atool # "compress" command in ranger
    curl
    emacs
    exfat
    file
    git
    gitAndTools.hub # github access for magithub
    gnumake
    gnupg
    imagemagick
    mpw
    nix-prefetch-git
    pass
    p7zip
    ranger
    silver-searcher # ag
    sudo
    tldr
    trash-cli
    unzip
    xclip # for pass
    xsel
    zip
  ];

  virtualisation.docker.enable = true;

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
