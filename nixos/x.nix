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
    conky
    i3status
    vanilla-dmz # cursor theme
    xorg.xmodmap     # swap l-ctrl and caps lock
  ];

  services.xserver = {
    # inherit dpi;

    enable = true;
    layout = "us";
    videoDrivers = [ "intel" ];

    displayManager = {
      lightdm.enable = true;
      # So urxvt knows where to find the socket.
      #export RXVT_SOCKET=/run/user/$(id -u)/urxvtd-socket
      sessionCommands = ''
        xrdb -merge ~/.Xresources
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
        MemoryLimit = "512M";
      };
    };

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

    twmn = mkGraphicalService {
      description = "twmn";
      serviceConfig = {
        ExecStart = "${pkgs.twmn}/bin/twmnd";
        Restart = "always";
        RestartSec = "5s";
        # MemoryLimit = "1024M";
      };
    };

    # TODO: try with high verbosity
    # dunst =
    #   let
    #     dunstrc = pkgs.writeText "dunstrc" ''
    #     '';

    #     wrapper-args = "-config ${dunstrc}";

    #     dunst-wrapper = pkgs.dunst.overrideAttrs (oldAttrs: {
    #       postInstall = oldAttrs.postInstall + ''
    #           wrapProgram $out/bin/dunst \
    #             --add-flags ${pkgs.lib.escapeShellArg wrapper-args}
    #         '';
    #     });
    #   in mkGraphicalService {
    #     description = "Dunst notification daemon";
    #     documentation = [ "man:dunst(1)" ];
    #     serviceConfig = {
    #       Type = "dbus";
    #       BusName = "org.freedesktop.Notifications";
    #       ExecStart = "${dunst-wrapper}/bin/dunst";
    #     };
    # };
  };
}
