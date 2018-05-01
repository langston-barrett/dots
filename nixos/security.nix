{ config, pkgs, ... }:

{
  # Encrypted DNS queries
  services.physlock.enable = true;
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

    # Can't be enabled in virtual guests
    rngd.enable = true; # feed hardware randomness to kernel when possible
    hideProcessInformation = true;
    # grsecurity = {
    #   enable = true;
    #   disableEfiRuntimeServices = true;
    #   lockTunables = true;
    #   trustedPathExecution = {
    #     enable = true;
    #     partialRestriction = true;
    #   };
    # };
    # manual: "if you have grsecurity enabled, you probably need this"
    #chromiumSuidSandbox.enable = true;
  };
}
