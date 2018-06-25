{ config, pkgs, ... }:

{
  # Encrypted DNS queries
  services.dnscrypt-proxy = {
    enable = true;
    resolverName = "cs-cawest"; # in California
  };

  # Opportunisticly encrypt TCP traffic
  networking.tcpcrypt.enable = true;

  security = {
    apparmor = {
      enable = true;
      confineSUIDApplications = true; # install apparmor profiles for SUID apps
    };

    hideProcessInformation = true;
  };
}
