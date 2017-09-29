{ config, pkgs, ... }:

let
  portland = {
    latitude = "45.480";
    longitude = "-122.63";
  };
  location = portland;
in
{
  imports = [];

  environment.systemPackages = with pkgs; [
    arc-theme
    paper-gtk-theme
    paper-icon-theme
    moka-icon-theme
    # TODO
    #xmodmap # swap l-ctrl and caps lock
    vanilla-dmz # cursor theme
  ];

  fonts = {
    enableFontDir = false;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      # fira-code
      # freefont-ttf        # use freeserif for TeX
      # ubuntu-font-family
      eb-garamond
      hack-font             # great monospaced font
      helvetica-neue-lt-std # classic
      lato
      libre-caslon
      noto-fonts-emoji      # emoji
      opensans-ttf
      overpass
      oxygenfonts           # UI
    ];
  };

  services.xserver = {
    enable = true;
    layout = "us";

    synaptics = {
      #enable = true;
      # OSX-like "Natural" two-finger scrolling
      twoFingerScroll = true;
      horizTwoFingerScroll = true;
      horizEdgeScroll = false;
      additionalOptions = ''
        Option "VertScrollDelta"  "-75"
        Option "HorizScrollDelta" "-75"
      '';
    };

    displayManager = {
      lightdm.enable = true;
      # So urxvt knows where to find the socket.
      #export RXVT_SOCKET=/run/user/$(id -u)/urxvtd-socket
      sessionCommands = ''
        xrdb -merge ~/.Xresources
        xmodmap ~/.xmodmap
      '';
    };
    desktopManager.xfce.enable = true;
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
  programs.light.enable = true;
}
