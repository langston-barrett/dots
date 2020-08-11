# Generally useful stuff, but not for e.g. servers
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dive # docker image size analysis
    exfat
    gitAndTools.hub # github access for magithub
    imagemagick
    pass

    # all necessary for emacs build epdfinfo
    autoconf
    automake
    autobuild
    pkgconfig
    gnum4

    # extra development
    clang
    rr
    shellcheck
  ] ++ lib.optional (pkgs ? "bat") pkgs.bat; # only in newer nixos

  apparmor.profiles =
    let writeDenyProfile =
          import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
    in [
      (writeDenyProfile { path = pkgs.imagemagick; binary = "convert"; })
    ];

  virtualisation.virtualbox.host.enable = true;
  services.physlock.enable = true;
}
