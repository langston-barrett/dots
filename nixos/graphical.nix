{ config, pkgs, ... }:

let
  portland = {
    latitude = 45.480;
    longitude = -122.63;
  };
  location = portland;
  master = (import ./pkgs.nix { inherit pkgs; }).master;
  variables = import ./hosts/this/variables.nix;
in {
  imports = [ ./gtk.nix ];

  environment.systemPackages = with pkgs; [
    # Window manager functionality
    feh
    rofi
    rxvt_unicode-with-plugins
    vanilla-dmz # cursor theme
    libnotify

    # General graphical packages
    alacritty
    anki
    baobab
    # kcolorchooser
    master.dropbox
    master.signal-desktop
    qutebrowser
    xsel
    zathura
  ];

  security.apparmor.profiles =
    let writeDenyProfile =
          import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
    in [
      (writeDenyProfile { path = pkgs.feh; binary = "feh"; })
      # TODO: test these
      # (writeDenyProfile { path = pkgs.kcolorchooser; binary = "kcolorchooser"; })
      # (writeDenyProfile { path = pkgs.xsel; binary = "xsel"; })
      # (writeDenyProfile { path = pkgs.zathura; binary = "zathura"; })
    ];

  fonts = {
    enableFontDir = false;
    enableGhostscriptFonts = true;
    fontconfig.dpi = 192;
    fonts = with pkgs; [
      # comfortaa
      # fira-code, freefont-ttf, ubuntu-font-family

      tex-gyre-bonum-math   # latex math font
      tex-gyre-schola-math  # latex math font
      tex-gyre-pagella-math # latex math font
      latinmodern-math      # latex math font

      carlito               # calibri compatible, for office
      eb-garamond
      powerline-fonts
      #hack-font             # great monospaced font
      #helvetica-neue-lt-std # classic
      fantasque-sans-mono   # https://computecuter.com/
      fira-code
      lato
      libre-caslon
      noto-fonts-emoji      # emoji
      opensans-ttf
      overpass              # header
      oxygenfonts           # UI
      stix-otf              # latex math font
      symbola
      unifont
    ];
  };

  # location = {
  #   latitude = location.latitude;
  #   longitude = location.longitude;
  # };

  services.redshift = {
    enable = false;
  };

  systemd.user.services = {
    feh = {
      enable = true;
      description = "Use feh image viewer to set the wallpaper";
      serviceConfig =
        let graphical = import ./functions/graphical-service.nix {
              inherit config;
              userService = true;
            };
        in graphical // {
          ExecStart = ''
            ${pkgs.feh}/bin/feh \
              --no-fehbg \
              --bg-fill \
              /home/${variables.username}/.config/wallpaper.jpg
          '';
          Restart = "on-failure";
          RestartSec = "5s";
          MemoryLimit = "512M";
        };
    };
  };
}
