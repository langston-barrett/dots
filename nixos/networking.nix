{ config, pkgs, ... }:

let kdeConnectPorts = [
      1714 1715 1716 1717 1718 1719 1720 1721 1722 1723 1724 1725 1726 1727
      1728 1729 1730 1731 1732 1733 1734 1735 1736 1737 1738 1739 1740 1741
      1742 1743 1744 1745 1746 1747 1748 1749 1750 1751 1752 1753 1754 1755
      1756 1757 1758 1759 1760 1761 1762 1763 1764 
    ];
in {

  networking = {
    nameservers = [ "8.8.8.8" "8.8.4.4" ];

    # Use plasma-nm or nm-applet as a GUI
    # Use gnome-control-center or nmtui to configure networkmanager
    networkmanager.enable = true;


    # wicd.enable = true;
    # wpa_supplicant
    # wireless = {
    #   enable = true;
    #   interfaces = [ "wlp1s0" ];
    # };

    # Open ports for KDE Connect
    firewall = {
      allowedTCPPorts = kdeConnectPorts;
      allowedUDPPorts = kdeConnectPorts;
    };
  };

  # To set up the VPN, copy the folders to /root/vpn.
  # Make sure to `chown -R root:root` and to `chmod 0600` them.

  services.openvpn.servers.galois-offsite = {
    autoStart = true;
    config = ''
      ca /root/vpn/glacier-udp-1198-langston/ca.crt

      dev tun
      tun-ipv6
      persist-tun
      persist-key
      cipher AES-256-CBC
      auth SHA256
      tls-client
      client
      resolv-retry infinite
      remote 65.100.53.74 1198 udp
      verify-x509-name "glacier.galois.com" name
      pkcs12 /root/vpn/glacier-udp-1198-langston/glacier-udp-1198-langston.p12
      tls-auth /root/vpn/glacier-udp-1198-langston/glacier-udp-1198-langston-tls.key 1
      remote-cert-tls server
      comp-lzo yes
    '';
    up = "echo nameserver $nameserver | ${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
    down = "${pkgs.openresolv}/sbin/resolvconf -d $dev";
  };

  services.openvpn.servers.galois-onsite = {
    autoStart = false;
    config = ''
      ca /root/vpn/glacier-udp-1199-langston/ca.crt

      dev tun
      tun-ipv6
      persist-tun
      persist-key
      cipher AES-256-CBC
      auth SHA256
      tls-client
      client
      resolv-retry infinite
      remote 192.168.40.7 1199 udp
      verify-x509-name "glacier.galois.com" name
      pkcs12 /root/vpn/glacier-udp-1199-langston/glacier-udp-1199-langston.p12
      tls-auth /root/vpn/glacier-udp-1199-langston/glacier-udp-1199-langston-tls.key 1
      remote-cert-tls server
    '';
    up = "echo nameserver $nameserver | ${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
    down = "${pkgs.openresolv}/sbin/resolvconf -d $dev";
  };
}
