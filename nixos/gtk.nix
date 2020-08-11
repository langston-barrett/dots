{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    arc-theme
    moka-icon-theme
    paper-gtk-theme
    paper-icon-theme
  ];

  environment.variables = {
    # GTK themes: Arc Light
    GTK_THEME = "Arc";
    GTK_PATH = "$GTK_PATH:${pkgs.arc-theme}/lib/gtk-2.0:${pkgs.arc-theme}/lib/gtk-3.0";
    GTK2_RC_FILES = "$HOME/.nix-profile/share/themes/Arc/gtk-2.0/gtkrc";
  };
}
