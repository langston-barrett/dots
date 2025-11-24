{ config, pkgs, ... }:

{
  networking = {
    # nameservers = [ "8.8.8.8" "8.8.4.4" "1.1.1.1" ];

    # Use plasma-nm or nm-applet as a GUI
    # Use gnome-control-center or nmtui to configure networkmanager
    networkmanager = {
      enable = true;
      # dns = "dnsmasq";
      appendNameservers = [
        # "8.8.8.8"
        # "8.8.4.4"
      ];
    };
  };
}
