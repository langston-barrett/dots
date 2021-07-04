{ config, pkgs, lib, ... }:

let hardened =
      (import <nixpkgs/nixos/modules/profiles/hardened.nix> {
        inherit config lib pkgs;
      });
in {
  # Encrypted DNS queries
  # services.dnscrypt-proxy = {
    # enable = true;
    # resolverName = "cs-cawest"; # in California
  # };

  # Opportunisticly encrypt TCP traffic
  # networking.tcpcrypt.enable = true;

  boot = {
    # Obscure network protocols and old or rare filesystems
    inherit (hardened.boot) blacklistedKernelModules;
  };

  security = {
    inherit (hardened.security)
      # "Restrict process information to the owning user."
      # Only on NixOS < 21.05
      # hideProcessInformation

      # Whether to prevent replacing the running kernel image.
      protectKernelImage;

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

    # auditd.enable = true;
    apparmor = {
      enable = true;
      # confineSUIDApplications = true; # install apparmor profiles for SUID apps
    };
  };
}
