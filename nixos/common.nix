# This files imports any others that are completely portable, i.e.
# should be used on any and every machine. It should be imported in each
# ./hosts/*.nix

{ config, pkgs, ... }:

let emacsVterm =
      with pkgs; (emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
        epkgs.emacs-libvterm
      ]);
in {
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

  services.emacs = {
    enable = true;
    package = emacsVterm;
  };
  nixpkgs.config.allowUnfree = true;

  # Time
  #services.chrony.enable = true;
  time.timeZone = "America/Los_Angeles";
  services.localtime.enable = true;
  # services.tzupdate.enable = true;

  nix = {
    maxJobs = 8;
    buildCores = 8;
    # gc.automatic = true;
    useSandbox = true;

    # Use a local clone of nixpkgs at /etc/nixpkgs
    #nixPath = [ "nixos-config=/etc/nixos/configuration.nix" "/home/siddharthist/code" ];

    # Use cachix binary caches
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
      # "http://fryingpan.dev.galois.com/hydra"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "fryingpan.dev.galois.com-1:z0mUoDyQ+o8E4PEIOGb3UaT40EAmSte7vidUvAJdjmU="
    ];
    trustedBinaryCaches = [
      "https://cache.nixos.org"
      # "http://fryingpan.dev.galois.com/hydra"
    ];
    trustedUsers = [ "root" "siddharthist" ];

  };
  system.autoUpgrade.enable = true;

  # only on recent nixos...
  # xdg = {
  #   icons.enable = true;
  #   menus.enable = true;
  #   mime.enable  = true;
  # };

  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: with ds; [ en ]))
    atool # "compress" command in ranger
    curl
    emacsVterm
    exfat
    fd
    file
    glibc-info
    git
    gitAndTools.hub # github access for magithub
    gnumake
    htop
    imagemagick
    mpw
    nix-prefetch-git
    p7zip
    pass
    ranger
    silver-searcher # ag
    sudo
    tldr
    trash-cli
    unzip
    xclip # for pass
    xsel
    zip
    zsh-completions
  ] ++ lib.optional (pkgs ? "bat") pkgs.bat; # only in newer nixos

  virtualisation.docker.enable = true;

  services.syncthing = {
    enable = false;
    group = "siddharthist";
    user = "siddharthist";
    dataDir = "/home/siddharthist/sync";
    openDefaultPorts = true;
  };
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
