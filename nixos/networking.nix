{ config, pkgs, ... }:

{
  boot.extraModulePackages = with config.boot.kernelPackages; [ wireguard ];
  # boot.kernelModules = [ "wireguard" ];

  networking = {
    wg-quick = {
      interfaces = {
        pdx =
          let awk = "${pkgs.gawk}/bin/awk";
          in {
          privateKeyFile = "/root/vpn/wg_pdx.private.key";
          address = [
            "10.20.10.87"
            "2001:428:6002:410::87"
          ];
          dns = ["10.20.10.1" "galois.com"];
          postUp = ''
            ip route add 65.132.32.180 via $(ip -o -4 route show to default | ${awk} '{print $3}') dev $(ip -o -4 route show to default | ${awk} '{print $5}')
          '';
          postDown = ''
            ip route del 65.132.32.180 via $(ip -o -4 route show to default | ${awk} '{print $3}') dev $(ip -o -4 route show to default | ${awk} '{print $5}')
          '';
          peers = [
            {
              publicKey = "bUVmd8sOGyhFD6MKe1r3Why2WscKxNdUhBUDBVA9zUo=";
              allowedIPs = [
                "192.168.48.0/20"
                "10.20.0.0/16"
                "65.132.32.128/25"
                "2001:428:6002:400::/56"
              ];
              endpoint = "65.132.32.180:31194";
            }
          ];
        };
      };
    };

    nameservers = [ "1.1.1.1" "8.8.8.8" "8.8.4.4" ];

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
