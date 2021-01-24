{ config, pkgs, ... }:

{

  networking = {

    # Use plasma-nm or nm-applet as a GUI
    # Use gnome-control-center or nmtui to configure networkmanager
    networkmanager = {
      enable = true;
      dns = "dnsmasq";
      appendNameservers = [
        "65.132.32.177"   #PDX
        "65.132.32.132"   #PDX
        "65.132.32.174"   #PDX
        "64.56.102.67"    #DAY
      ];
    };

    # wicd.enable = true;
    # wpa_supplicant
    # wireless = {
    #   enable = true;
    #   interfaces = [ "wlp1s0" ];
    # };

    # Open ports for KDE Connect
    firewall = {
      allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
      allowedTCPPortRanges = [
        { from = 1714; to = 1764; }
        # Herms server, other misc:
        { from = 9000; to = 9003; }
      ];
    };
  };
}
