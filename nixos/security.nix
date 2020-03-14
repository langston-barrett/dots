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

    # auditd.enable = true;
    apparmor = {
      enable = true;
      confineSUIDApplications = true; # install apparmor profiles for SUID apps
      profiles =
        let mkDenyProfile = path: binary: ''
              #include <tunables/global>
              profile ${binary} ${path}/bin/${binary} {
                #include <abstractions/base>

                deny network,
                deny mount,
                deny umount,

                # Deny access by default
                deny /boot/** rwklx,
                deny /root/** rwklx,
                deny /sys/** rwklx,
                deny @{PROC}/** rwklx,

                # Allow access to a few directories
                /tmp/** rwmix,
                /run/user/** rwmix,
                /nix/store/** rwmix,
                @{HOME}/** rw,
              }
            '';
            writeDenyProfile = path: binary:
              pkgs.writeText "apparmor-${binary}" (mkDenyProfile path binary);
        in [
          (writeDenyProfile pkgs.arandr "arandr")
          (writeDenyProfile pkgs.feh "feh")
          (writeDenyProfile pkgs.file "file")
          (writeDenyProfile pkgs.imagemagick "convert")
          (writeDenyProfile pkgs.mpw "mpw")
          (writeDenyProfile pkgs.p7zip "7z")
          (writeDenyProfile pkgs.unzip "unzip")
          (writeDenyProfile pkgs.xcompmgr "xcompmgr")
          (writeDenyProfile pkgs.zip "zip")
          # Rofi needs /run/user
        ];
    };
    hideProcessInformation = true;
  };
}
