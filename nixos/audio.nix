{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pavucontrol
  ];

  # sound.enableMediaKeys = true; # sxhkd takes care of this
  # see https://github.com/NixOS/nixpkgs/issues/39635
  # TODO: see logs
  hardware.pulseaudio = {
    # enable = true;
    # package = pkgs.pulseaudioFull;
    # systemWide = true;
  };
}
