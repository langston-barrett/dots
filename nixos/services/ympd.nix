{ config, lib, pkgs, ... }:

# TODO! Upstream security improvements to nixpkgs

with lib;

let
  cfg = config.services.myympd;
in {

  ###### interface

  options = {

    services.myympd = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ympd, the MPD Web GUI.";

      };

      webPort = mkOption {
        type = types.string;
        default = "8080";
        description = "The port where ympd's web interface will be available.";
        example = "ssl://8080:/path/to/ssl-private-key.pem";

      };

      mpd = {
        host = mkOption {
          type = types.string;
          default = config.services.mpd.network.listenAddress;
          description = "The host where MPD is listening.";
          example = "localhost";

        };

        port = mkOption {
          type = types.int;
          default = config.services.mpd.network.port;
          description = "The port where MPD is listening.";
          example = 6600;

        };

      };


    };


  };


  ###### implementation

  config = mkIf cfg.enable {
    systemd.services.ympd = {
      description = "Standalone MPD Web GUI written in C";
      wantedBy = [ "multi-user.target" ];
      confinement.enable = true;
      serviceConfig = {
        ExecStart = "${pkgs.ympd}/bin/ympd --host ${cfg.mpd.host} --port ${toString cfg.mpd.port} --webport ${cfg.webPort}";
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateMounts = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        RestrictAddressFamilies = "AF_INET AF_INET6 AF_UNIX AF_NETLINK";
        RestrictNamespaces = true;
      };

    };


  };

}
