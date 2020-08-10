{ config, pkgs, ... }:

# TODO deduplicate
let mkGraphicalService = attrs: {
    enable = true;
    environment.DISPLAY = ":${builtins.toString config.services.xserver.display}";
    # environment = { DISPLAY = ":0"; };
    after = [ "display-manager.service" ];
    partOf = [ "display-manager.service" ];
    wantedBy = [ "graphical.target" ];
  } // attrs;
in {
  imports = [ ./x.nix ];

  services.xserver = {
    displayManager = {
      lightdm.enable = true;
      sessionCommands = ''
        xrdb -merge ~/.Xresources
      '';
      defaultSession = "none+i3";
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
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

    twmn = mkGraphicalService {
      enable = false;
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
