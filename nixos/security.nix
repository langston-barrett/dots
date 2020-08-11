{ config, pkgs, ... }:

{
  # Encrypted DNS queries
  # services.dnscrypt-proxy = {
    # enable = true;
    # resolverName = "cs-cawest"; # in California
  # };

  # Opportunisticly encrypt TCP traffic
  # networking.tcpcrypt.enable = true;

  security = {

    polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.systemd1.manage-units") {
                if (action.lookup("unit") == "physlock.service") {
                    var verb = action.lookup("verb");
                    if (verb == "start") {
                        return polkit.Result.YES;
                    }
                }
            }
        });
      '';
    };

    sudo = {
      enable = true;
    };

    # "Restrict process information to the owning user."
    hideProcessInformation = true;

    # auditd.enable = true;
    apparmor = {
      enable = true;
      confineSUIDApplications = true; # install apparmor profiles for SUID apps
    };
  };
}
