# Create an Apparmor profile that denies access to most of the filesystem,
# mounting, and network
{ pkgs ? import <nixpkgs> { }
, writeText ? pkgs.writeText
}:

{ path
, binary
# Options:
# Default to deny:
, network ? false
, mount ? false
, umount ? false
, boot ? false
, root ? false
, sys ? false
, proc ? false
# Default to allow:
, tmp ? true
, run ? true
, store ? true
, home ? true
}:
writeText "apparmor-${binary}" ''
  #include <tunables/global>
  profile ${binary} ${path}/bin/${binary} {
    #include <abstractions/base>

    # Default to deny (capabilities)
    ${if network then "" else "deny network,"}
    ${if mount then "" else "deny mount,"}
    ${if umount then "" else "deny umount,"}

    # Default to deny (filesystem)
    ${if boot then "" else "deny /boot/** rwklx,"}
    ${if root then "" else "deny /root/** rwklx,"}
    ${if sys then "" else "deny /sys/** rwklx,"}
    ${if proc then "" else "deny @{PROC}/** rwklx,"}

    # Default to allow
    ${if !tmp then "" else "/tmp/** rwmix,"}
    ${if !run then "" else "/run/user/** rwmix,"}
    ${if !store then "" else "/nix/store/** rwmix,"}
    ${if !home then "" else "@{HOME}/** rw,"}
  }
''
