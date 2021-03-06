# Generally useful stuff, but not for e.g. servers
{ config, pkgs, ... }:

{
  imports = [ ./dev.nix ];

  environment.systemPackages = with pkgs; [
    exfat
    graphviz
    imagemagick
    pass

    # all necessary for emacs build epdfinfo
    autoconf
    automake
    autobuild
    pkgconfig
    gnum4
  ];

  # security.apparmor.profiles =
  #   let writeDenyProfile =
  #         import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
  #   in [
  #     (writeDenyProfile { path = pkgs.bat; binary = "bat"; })
  #     (writeDenyProfile { path = pkgs.imagemagick; binary = "convert"; })
  #     (writeDenyProfile { path = pkgs.pass; binary = "pass"; })
  #   ];

  virtualisation.virtualbox.host.enable = true;
  services.physlock.enable = true;
}
