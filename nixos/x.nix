{ config, pkgs, ... }:

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
      # these replace .xsession
      #export DISPLAY=:0.0
      # screen dpi
      #xrandr --output eDP1 --dpi 192
      # correct X settings
      # So urxvt knows where to find the socket.
      #export RXVT_SOCKET=/run/user/$(id -u)/urxvtd-socket
      sessionCommands = ''
        xrdb -merge ~/.Xresources
        xmodmap ~/.xmodmap
      '';
    };
    #desktopManager.kde5.enable = true;
    desktopManager.gnome3.enable = true;
    desktopManager.xfce.enable = true;
    desktopManager.lxqt.enable = true;
    windowManager.i3.enable = true;
    windowManager.i3.package = pkgs.i3-gaps;
    monitorSection = ''
      DisplaySize   294 166
    '';
  };

  services.redshift = {
    enable = true;
    # Portland
    latitude = "45.480";
    longitude = "-122.63";
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
