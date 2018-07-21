{ config, pkgs, ... }:

let mkGraphicalService = attrs: {
    enable = true;
    # user = "siddharthist";
    environment = { DISPLAY = ":0"; };
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
    conky
    i3status
    vanilla-dmz # cursor theme
    xorg.xmodmap     # swap l-ctrl and caps lock
  ];

  services.xserver = {
    dpi = 192;
    enable = true;
    layout = "us";

    displayManager = {
      lightdm.enable = true;
      # So urxvt knows where to find the socket.
      #export RXVT_SOCKET=/run/user/$(id -u)/urxvtd-socket
      sessionCommands = ''
        xrdb -merge ~/.Xresources
        xmodmap ~/.xmodmap
      '';
    };
    # desktopManager.xfce.enable = true;
    windowManager.i3.enable = true;
    windowManager.i3.package = pkgs.i3-gaps;
    monitorSection = ''
      DisplaySize   294 166
    '';

    # TODO: get rid of ~/.xmodmap and see if this works
    xkbOptions = "ctrl:swapcaps";
  };

  systemd.user.services = {
    xcompmgr = mkGraphicalService {
      description = "Use xcompmgr compositor";
      serviceConfig = {
        ExecStart = "${pkgs.xcompmgr}/bin/xcompmgr -c";
        Restart = "always";
        RestartSec = "5s";
      };
    };

    kdeconnect = mkGraphicalService {
      description = "kdeconnect";
      serviceConfig = {
        ExecStart = "${pkgs.kdeconnect}/bin/kdeconnect-indicator";
        Restart = "always";
        RestartSec = "5s";
      };
    };
  };
}
