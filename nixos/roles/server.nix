# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  services = {
    openssh = {
      enable = true;
      forwardX11 = true;
    };
    xrdp = {
      enable = true;
      port = 3389;
    };
  };
  networking.firewall.allowedTCPPorts = [ 3389 ];
}
