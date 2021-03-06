# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../roles/laptop.nix
    ../../steam.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Crypto!!
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/445eed41-be61-44fa-9cd0-ffea26dea921";
      preLVM = true;
      allowDiscards = true;
    };
  };

  networking.hostName = "langston-nixos";

  programs.java.enable = true;

  environment.systemPackages = with pkgs; [
    # beets
    calibre
    gimp
    keynav
    kdeconnect
    maim
    mu
    spotify
    tmux
    vlc
    ympd

    # python development
    # python
    # pythonPackages.importmagic
  ];

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
    # up = "echo nameserver $nameserver | ${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
    # down = "${pkgs.openresolv}/sbin/resolvconf -d $dev";
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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
