{ config, pkgs, ... }:

{
  imports = [ ./x.nix ];

  environment.systemPackages = with pkgs; [
    (polybar.override {
      i3GapsSupport = true;
      i3-gaps = i3-gaps;
      jsoncpp = jsoncpp;
    })
    # i3status
  ];

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

  services.picom = {
    enable = true;
  };
  services.unclutter.enable = true;

  security.apparmor.profiles =
    let writeDenyProfile =
          import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
    in [
      (writeDenyProfile { path = pkgs.xcompmgr; binary = "xcompmgr"; })
    ];

  systemd.user.services =
    let graphical = import ./functions/graphical-service.nix {
          inherit config;
          userService = true;
        };
    in {

    xcompmgr = {
      description = "Use xcompmgr compositor";
      serviceConfig = graphical // {
        enable = false;
        ExecStart = "${pkgs.xcompmgr}/bin/xcompmgr -c";
        Restart = "always";
        RestartSec = "5s";
        MemoryLimit = "512M";
      };
    };

    twmn = {
      enable = false;
      description = "twmn notification server daemon";
      serviceConfig = graphical // {
        ExecStart = "${pkgs.twmn}/bin/twmnd";
        Restart = "always";
        RestartSec = "5s";
        MemoryLimit = "1024M";
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
