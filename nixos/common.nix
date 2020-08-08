# This files imports any others that are completely portable, i.e.
# should be used on any and every machine. It should be imported in each
# ./hosts/*.nix

{ config, pkgs, ... }:

let variables = import ./hosts/this/variables.nix;
    myEmacs =
      with pkgs; (emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
        #epkgs.emacsql-sqlite
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
    package = myEmacs;
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
    trustedUsers = [ "root" variables.username ];

  };
  system.autoUpgrade.enable = true;

  # only on recent nixos...
  # xdg = {
  #   icons.enable = true;
  #   menus.enable = true;
  #   mime.enable  = true;
  # };

  documentation = {
    dev.enable = true;
    man.enable = true;
    nixos.enable = true;
  };

  environment.systemPackages = with pkgs; [
    acpi # battery monitoring in scripts
    myEmacs
    wget # org-board
  ] ++ import ./minimal.nix { inherit pkgs; };

  virtualisation.docker.enable = true;

  services.syncthing = {
    enable = false;
    group = variables.username;
    user = variables.username;
    dataDir = "/home/${variables.username}/sync";
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
