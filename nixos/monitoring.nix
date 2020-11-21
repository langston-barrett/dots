{ config, pkgs, ... }:

# https://christine.website/blog/prometheus-grafana-loki-nixos-2020-11-20
# sudo grafana-cli --homepath /var/lib/grafana admin reset-admin-password admin

{
  services.grafana = {
    enable = true;
    domain = "grafana";
    port = 2342;
    addr = "127.0.0.1";
    security.adminUser = "admin";
    security.adminPassword = "admin";
    users.allowSignUp = true;
    dataDir = "/var/lib/grafana";
    provision.datasources = [
      # May require manual setup too
      {
        access = "direct";
        isDefault = true;
        type = "prometheus";
        url = "http://localhost:9001";
      }
      {
        access = "direct";
        type = "loki";
        url = "http://localhost:3100";
      }
    ];
  };


  services.loki = {
    enable = true;
    configFile = ./files/loki.yaml;
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    scrapeConfigs = [
      {
        job_name = "big";
        static_configs = [
          {
            targets = [
              "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
            ];
          }
        ];
      }
    ];
  };
  systemd.services.promtail = {
    description = "Promtail service for Loki";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.grafana-loki}/bin/promtail --config.file ${./files/promtail.yaml}
      '';
    };
  };

  # nginx reverse proxy
  services.nginx.virtualHosts.${config.services.grafana.domain} = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
      proxyWebsockets = true;
    };
  };
}
