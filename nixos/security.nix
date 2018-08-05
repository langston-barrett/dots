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
                /nix/store/** rwmix,
                @{HOME}/** rw,
              }
            '';
            writeDenyProfile = path: binary:
              pkgs.writeText "apparmor-${binary}" (mkDenyProfile path binary);
        in [
          (writeDenyProfile pkgs.feh "feh")
          (writeDenyProfile pkgs.file "file")
          (writeDenyProfile pkgs.imagemagick "convert")
          (writeDenyProfile pkgs.mpw "mpw")
          (writeDenyProfile pkgs.p7zip "7z")
          (writeDenyProfile pkgs.unzip "unzip")
          (writeDenyProfile pkgs.zip "zip")
        ];
    };
    hideProcessInformation = true;

  };
}
