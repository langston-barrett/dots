{ config, pkgs, ... }:

let
  portland = {
    latitude = "45.480";
    longitude = "-122.63";
  };
  location = portland;
in {

  environment.systemPackages = with pkgs; [
    # GTK and window manager functionality
    arc-theme
    feh
    moka-icon-theme
    paper-gtk-theme
    paper-icon-theme
    rofi
    rxvt_unicode-with-plugins
    vanilla-dmz # cursor theme
    xfce.xfce4_power_manager_gtk3

    # General graphical packages
    firefox
    kcolorchooser
    xpdf
    zathura
  ];

  fonts = {
    enableFontDir = false;
    enableGhostscriptFonts = true;
    fontconfig.dpi = 192;
    fonts = with pkgs; [
      # fira-code, freefont-ttf, ubuntu-font-family

      # These actually require the new package set
      tex-gyre-bonum-math   # latex math font
      tex-gyre-schola-math  # latex math font
      tex-gyre-pagella-math  # latex math font
      latinmodern-math      # latex math font

      # These don't
      eb-garamond
      hack-font             # great monospaced font
      #helvetica-neue-lt-std # classic
      lato
      libre-caslon
      noto-fonts-emoji      # emoji
      opensans-ttf
      overpass              # header
      oxygenfonts           # UI
      stix-otf              # latex math font
    ];
  };

  services.redshift = {
    enable = true;
    # Portland
    latitude = location.latitude;
    longitude = location.longitude;
  };

  environment.variables = {
    # GTK themes: Arc Light
    GTK_THEME = "Arc";
    GTK_PATH = "$GTK_PATH:${pkgs.arc-theme}/lib/gtk-2.0:${pkgs.arc-theme}/lib/gtk-3.0";
    GTK2_RC_FILES = "$HOME/.nix-profile/share/themes/Arc/gtk-2.0/gtkrc";
  };

  systemd.user.services = {
    feh = {
      enable = true;
      description = "Use feh image viewer to set the wallpaper";
      after = [ "display-manager.service" ];
      partOf = [ "display-manager.service" ];
      wantedBy = [ "graphical.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.feh}/bin/feh --no-fehbg --bg-fill /home/siddharthist/.config/wallpaper.jpg";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };
  };
}