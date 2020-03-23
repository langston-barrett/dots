{ config, pkgs, ... }:

{
  # sound.enableMediaKeys = true; # sxhkd takes care of this
  # see https://github.com/NixOS/nixpkgs/issues/39635
  # TODO: see logs
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    # tcp = {
    #   enable = true;
    #   anonymousClients.allowedIpRanges = [ "127.0.0.1" ];
    # };
    # configFile = pkgs.writeText "default.pa" ''
    #   load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
    # '';
    # systemWide = true;
  };
}
