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
      forwardX11 = true;  # notifications
      ports = [ 22 ];  # TODO: change
      permitRootLogin = "no";
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
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

  systemd.user.services = {
    tigervnc = {
      # TODO: Find which setting makes it so tigervnc can't drop capabilities
      enable = false;
      description = "tigervnc";
      serviceConfig =
        import ../functions/service.nix { userService = true; } // {
          ExecStart = "${pkgs.tigervnc}/bin/vncserver";
          Restart = "always";
          RestartSec = "5s";
          MemoryLimit = "4G";
          MemorySwapMax = "0M";
          PrivateNetwork = false;
          NoNewPrivileges = false; # TODO maybe remove?
        };
    };
  };

  networking.firewall.allowedTCPPorts = [
    5500 # tigervnc
    5901 # tigervnc
  ];
}
