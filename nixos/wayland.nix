# UNFINISHED
{ config, pkgs, ... }:
{
  imports = [ ./graphical.nix ];

  hardware.opengl.enable = true;

  programs.sway = {
    enable = true;
    extraSessionCommands = ''
      export XKB_DEFAULT_OPTIONS=ctrl:nocaps,
    '';
  };

  programs.way-cooler = {
    enable = false;
    enableBar = true;
  };
}
