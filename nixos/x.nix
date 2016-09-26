{ config, pkgs, ... }:

{
  imports = [];

  environment.systemPackages = with pkgs; [
    arc-gtk-theme
    numix-icon-theme-circle
  ];

  services.xserver = {
    enable = true;
    layout = "us";

    synaptics = {
      enable = true;
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
      slim = {
        enable = true;
        defaultUser = "siddharthist";
      };
    };

    # multitouch.enable = true;

    windowManager.i3-gaps.enable = true;
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
    # GTK themes: Arc Dark
    #GTK_THEME = "Arc-Dark";
    #GTK_PATH = "$GTK_PATH:${pkgs.arc-gtk-theme}/lib/gtk-2.0:${pkgs.arc-gtk-theme}/lib/gtk-3.0";
    #GTK2_RC_FILES = "$HOME/.nix-profile/share/themes/Arc-Dark/gtk-2.0/gtkrc";

    # GTK themes: Arc Light
    GTK_THEME = "Arc";
    GTK_PATH = "$GTK_PATH:${pkgs.arc-gtk-theme}/lib/gtk-2.0:${pkgs.arc-gtk-theme}/lib/gtk-3.0";
    GTK2_RC_FILES = "$HOME/.nix-profile/share/themes/Arc/gtk-2.0/gtkrc";
  };

  services.xbanish.enable = true;
  #services.urxvtd.enable = true;
  programs.light.enable = true;
}
