{ config, pkgs, ... }:

let
  portland = {
    latitude = "45.480";
    longitude = "-122.63";
  };
  location = portland;
in {
  environment.systemPackages = with pkgs; [
    arc-theme
    conky
    feh
    i3status
    moka-icon-theme
    paper-gtk-theme
    paper-icon-theme
    rxvt_unicode-with-plugins
    vanilla-dmz # cursor theme
    xfce.xfce4_power_manager_gtk3
    xorg.xmodmap     # swap l-ctrl and caps lock
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

  services.xserver = {
    dpi = 192;
    enable = true;
    layout = "us";

    displayManager = {
      lightdm.enable = true;
      # So urxvt knows where to find the socket.
      #export RXVT_SOCKET=/run/user/$(id -u)/urxvtd-socket
      sessionCommands = ''
        xrdb -merge ~/.Xresources
        xmodmap ~/.xmodmap
      '';
    };
    # desktopManager.xfce.enable = true;
    windowManager.i3.enable = true;
    windowManager.i3.package = pkgs.i3-gaps;
    monitorSection = ''
      DisplaySize   294 166
    '';

    # TODO: get rid of ~/.xmodmap and see if this works
    xkbOptions = "ctrl:swapcaps";
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

  services.xbanish.enable = true;
  #services.urxvtd.enable = true;

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
