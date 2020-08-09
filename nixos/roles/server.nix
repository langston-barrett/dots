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

    # View with Remmina
    xrdp = {
      enable = true;
      port = 3389;
    };
  };

  # View with tigervnc
  environment.systemPackages = with pkgs; [ tigervnc ];

  networking.firewall.allowedTCPPorts = [
    3389
    5500 # tigervnc
    5901 # tigervnc
  ];
}
