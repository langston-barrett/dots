# * X server
{ config
, pkgs
, dpi ? 92
, ... }:

let mkGraphicalService = attrs: {
    enable = true;
    environment.DISPLAY = ":${builtins.toString config.services.xserver.display}";
    # environment = { DISPLAY = ":0"; };
    after = [ "display-manager.service" ];
    partOf = [ "display-manager.service" ];
    wantedBy = [ "graphical.target" ];
  } // attrs;
in {
  imports = [ ./graphical.nix ];

  environment.systemPackages = with pkgs; [
    # Having to do with X/i3 functionality
    arc-theme
    arandr
    # conky
    # i3status
    vanilla-dmz # cursor theme
    xorg.xmodmap     # swap l-ctrl and caps lock
  ];

  systemd.user.services = {
    # kdeconnect.service: Main process exited, code=killed, status=6/ABRT
    # kdeconnect = mkGraphicalService {
    #   description = "kdeconnect";
    #   serviceConfig = {
    #     ExecStart = "${pkgs.kdeconnect}/bin/kdeconnect-indicator";
    #     Restart = "always";
    #     RestartSec = "5s";
    #     MemoryLimit = "512M";
    #   };
    # };
  };
}
