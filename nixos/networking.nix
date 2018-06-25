{ config, pkgs, ... }:

{
  networking = {
    nameservers = [ "8.8.8.8" "8.8.4.4" ];

    # wicd.enable = true;
    # wpa_supplicant
    # wireless = {
    #   enable = true;
    #   interfaces = [ "wlp1s0" ];
    # };

    # Use gnome-control-center or nmtui to configure networkmanager
    networkmanager.enable = true;
  };
}
