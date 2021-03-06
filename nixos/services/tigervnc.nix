{ config, lib, pkgs, ... }:

# TODO! Upstream

with lib;

let
  cfg = config.services.tigervnc;
in {

  ###### interface

  options = {
    services.tigervnc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the tigervnc VNC server.";
      };

      port = mkOption {
        type = types.int;
        default = 5900;
        description = "The port to listen for connections from viewers";
      };

      localhost = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Only allow connections from the same machine.

          Disabling this option is definitely insecure, this module does not yet support any form of authentication and encryption. It is recommended that you use SSH tunneling to access the VNC server.
        '';
      };

      geometry = mkOption {
        type = types.nullOr types.string;
        default = null;
        example = "1920x1080";
        description = "Size of the desktop to be created.";
      };

      noXStartup = mkOption {
        type = types.bool;
        default = false;
        description = "Size of the desktop to be created.";
      };

      xStartup = mkOption {
        type = types.nullOr types.string;
        default = null;
        description = "Run a custom startup script, instead of %HOME/.vnc/xstartup, after launching Xvnc.";
      };
    };
  };


  ###### implementation

  config = mkIf cfg.enable {

    users.users.tigervnc = {
      home = "/var/run/tigervnc";
      createHome = true;
    };

    # For some reason, this can't be a user service?
    # tigervnc.service: Failed at step CAPABILITIES spawning /nix/store/migv2yykkb49sqrj5ng5c43igsbnvpns-tigervnc-1.10.1/bin/vncserver: Operation not permitted
    systemd.services.tigervnc = {
      description = "A VNC server";
      wantedBy = [ "multi-user.target" ];
      after = ["network.target"];
      environment.HOME = "/var/run/tigervnc";
      serviceConfig = {
        # Log to stderr so the logs are visible via systemctl/journalctl
        ExecStart = ''
          ${pkgs.tigervnc}/bin/vncserver \
            ${optionalString (cfg.geometry != null) "-geometry ${cfg.geometry}"} \
            ${optionalString cfg.localhost "-localhost"} \
            ${optionalString (cfg.xStartup != null) "-xstartup ${cfg.xStartup}"} \
            ${optionalString cfg.noXStartup "-noxstartup"} \
            -rfbport ${builtins.toString cfg.port} \
            -Log *:stderr:30 \
            -SecurityTypes None
        '';
        Restart = "always";
        RestartSec = "5s";
        Type = "forking";
        WorkingDirectory = "/var/run/tigervnc";
        User = "tigervnc";

        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateMounts = true;
        PrivateTmp = true;
        ProtectControlGroups = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        RestrictAddressFamilies = "AF_INET AF_INET6 AF_UNIX AF_NETLINK";
        RestrictNamespaces = true;
      };
    };
  };
}
