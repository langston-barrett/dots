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

  environment.systemPackages = with pkgs; [
    myEmacs
  ] ++ import ./minimal.nix { inherit pkgs; };

  services.emacs = {
    enable = true;
    package = myEmacs;
  };

  # Time
  time.timeZone = "America/Los_Angeles";
  services.localtime.enable = true;

  # Nix
  nixpkgs.config.allowUnfree = true;
  system.autoUpgrade.enable = true;
  nix = {
    buildCores = 0; # All available
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

  xdg = {
    icons.enable = true;
    menus.enable = true;
    mime.enable  = true;
  };

  documentation = {
    dev.enable = true;
    man.enable = true;
    nixos.enable = true;
  };

  virtualisation.docker.enable = true;

  services.syncthing = {
    enable = false;
    group = variables.username;
    user = variables.username;
    dataDir = "/home/${variables.username}/sync";
    openDefaultPorts = true;
  };
}
