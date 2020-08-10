# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  # View with tigervnc
  environment.systemPackages = with pkgs; [
    tigervnc
    tmux
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

  networking.firewall.allowedTCPPorts = [
    3389
    5500 # tigervnc
    5901 # tigervnc
  ];
}
