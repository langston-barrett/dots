# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
    # ../monitoring.nix
  ];

  environment.systemPackages = with pkgs; [
    # tigervnc
    tmux
  ];

  services = {
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KdbInteractiveAuthentication = false;
      };
      ports = [ 9988 ];
    };

    # View with Remmina
    # Unforunately, this can't be made secure with an SSH tunnel, the service
    # doesn't expose an option to limit the listening to localhost.
    xrdp = {
      enable = false;
      port = 3389;
      defaultWindowManager = "i3";  # default: xterm
    };
  };
}
