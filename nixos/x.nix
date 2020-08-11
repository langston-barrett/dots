# * X server
{ config
, pkgs
, ...
}:

{
  imports = [ ./graphical.nix ];

  services.xserver.enable = true;

  environment.systemPackages = with pkgs; [
    # Having to do with X/i3 functionality
    arc-theme
    arandr
    # conky
    vanilla-dmz # cursor theme
    xorg.xmodmap # swap l-ctrl and caps lock
  ];

  # TODO: Factor out into a module, open ports in firewall automatically
  systemd.user.services = {
    # kdeconnect.service: Main process exited, code=killed, status=6/ABRT
    kdeconnect = {
      enable = false;
      description = "kdeconnect";
      serviceConfig =
        let graphical = import ./functions/graphical-service.nix {
              inherit config;
              userService = true;
            };
        in graphical // {
          ExecStart = "${pkgs.kdeconnect}/bin/kdeconnect-indicator";
          Restart = "always";
          RestartSec = "5s";
          MemoryLimit = "512M";
          PrivateNetwork = false;
        };
    };
  };
}
