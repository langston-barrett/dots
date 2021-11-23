# This files imports any others that are completely portable, i.e.
# should be used on any and every machine. It should be imported in each
# ./hosts/*.nix

{ config, pkgs, ... }:

let variables = import ./hosts/this/variables.nix;
in {
  imports = [
    ./security.nix
    ./users.nix
    ./zsh.nix
  ];

  # Use a bigger font for HiDPI displays
  console = {
    keyMap = "us";
    font = "sun12x22";
  };

  i18n = {
    # consoleFont = "sun12x22";
    # consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
  ] ++ import ./minimal.nix { inherit pkgs; };

  # Time
  time.timeZone = "America/New_York";
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
      # "https://hie-nix.cachix.org"
      # "http://fryingpan.dev.galois.com/hydra"
    ];
    binaryCachePublicKeys = [
      # "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
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
    man = {
      enable = true;
      generateCaches = true;
    };
    nixos.enable = true;
  };

  virtualisation = {
    docker = {
      enable = false;
      extraOptions = "--dns 8.8.8.8 --dns 8.8.4.4";
    };
    podman = {
      enable = true;
      dockerCompat = true;
      dockerSocket = {
        enable = true;
      };
    };
  };

  services.syncthing = {
    enable = false;
    group = variables.username;
    user = variables.username;
    dataDir = "/home/${variables.username}/sync";
    openDefaultPorts = true;
  };

  # TODO: Fix after NixOS 21.05 upgrade
  # Apparmor profiles for package in ./minimal.nix
  # security.apparmor.profiles =
  #   let writeDenyProfile =
  #         import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
  #   in [
  #     (writeDenyProfile { path = pkgs.curl; binary = "curl"; })
  #     (writeDenyProfile { path = pkgs.fasd; binary = "fasd"; })
  #     (writeDenyProfile { path = pkgs.file; binary = "file"; })
  #     (writeDenyProfile { path = pkgs.jq; binary = "jq"; })
  #     (writeDenyProfile { path = pkgs.mpw; binary = "mpw"; })
  #     (writeDenyProfile { path = pkgs.p7zip; binary = "7z"; })
  #     (writeDenyProfile { path = pkgs.pet; binary = "pet"; })
  #     (writeDenyProfile { path = pkgs.tree; binary = "tree"; })
  #     (writeDenyProfile { path = pkgs.unzip; binary = "unzip"; })
  #     (writeDenyProfile { path = pkgs.zip; binary = "zip"; })

  #     # TODO debug
  #     # Failed to open /dev/tty
  #     # (writeDenyProfile { path = pkgs.fzf; binary = "fzf"; })

  #     # TODO test these
  #     # (writeDenyProfile { path = pkgs.trash-cli; binary = "trash-empty"; })
  #     # (writeDenyProfile { path = pkgs.trash-cli; binary = "trash-put"; })
  #     # (writeDenyProfile { path = pkgs.trash-cli; binary = "trash-restore"; })
  #     # (writeDenyProfile {
  #     #   path = pkgs.tldr;
  #     #   binary = "tldr";
  #     #   network = true;
  #     # })
  #   ];
}
