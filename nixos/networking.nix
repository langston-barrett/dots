{ config, pkgs, ... }:

{
  # boot.extraModulePackages = with config.boot.kernelPackages; [ wireguard ];
  # boot.kernelModules = [ "wireguard" ];

  networking = {
    wg-quick = {
      interfaces = {
        galois =
          let awk = "${pkgs.gawk}/bin/awk";
          in {
          privateKeyFile = "/root/vpn/galois.private.key";
          address = [ "10.100.10.87" "fd00:428:6002:410::87/64" ];
          dns = ["10.100.10.1" "10.100.10.2" "fd00:428:6002:410::2" "galois.com"];
          peers = [
            {
              publicKey = "9xqBm/8JNWWyfB2CG3LtETxO4LZJqvnu9HidfJXSukk=";
              allowedIPs = [
                "10.100.0.0/32"
                "10.10.0.0/16"
                "64.16.52.128/25"
              ];
              endpoint = "wg1.galois.com:31196";
            }
            {
              publicKey = "bUVmd8sOGyhFD6MKe1r3Why2WscKxNdUhBUDBVA9zUo=";
              allowedIPs = [
                "10.100.10.2/32"
                "10.20.0.0/16"
                "64.16.53.128/25"
                "192.168.26.0/24"
                "192.168.48.0/20"
                "2001:428:6002:400::/56"
                "fd00:428:6002:410::/64"
              ];
              endpoint = "wg2.galois.com:31196";
            }
          ];
        };
      };
    };

    # nameservers = [ "8.8.8.8" "8.8.4.4" "1.1.1.1" ];

    # Use plasma-nm or nm-applet as a GUI
    # Use gnome-control-center or nmtui to configure networkmanager
    networkmanager = {
      enable = true;
      dns = "dnsmasq";
      appendNameservers = [
        # "8.8.8.8"
        # "8.8.4.4"
        "10.20.10.1"
        "galois.com"
        # "65.132.32.177"   #PDX
        # "65.132.32.132"   #PDX
        # "65.132.32.174"   #PDX
        # "64.56.102.67"    #DAY
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
