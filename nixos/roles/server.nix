# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  environment.systemPackages = with pkgs; [
    tigervnc
    tmux
  ];

  services = {
    openssh = {
      enable = true;
      # forwardX11 = true;
      ports = [ 22 ];  # TODO: change
      permitRootLogin = "no";
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    # View with Remmina
    # TODO: localhost + SSH tunnel
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
