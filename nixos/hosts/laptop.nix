# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration-laptop.nix
    ../common.nix

    ../audio.nix
    ../mail.nix
    ../networking.nix
    ../steam.nix
    ../x.nix
    # ../wayland.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Crypto!!
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/445eed41-be61-44fa-9cd0-ffea26dea921";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  networking.hostName = "langston-nixos"; # Define your hostname.
  # networking.networkmanager.enable = true;

  services.xserver = {
    synaptics = {
      enable = true;
      # OSX-like "Natural" two-finger scrolling
      twoFingerScroll = true;
      horizTwoFingerScroll = true;
      horizEdgeScroll = false;
      additionalOptions = ''
        Option "VertScrollDelta"  "-75"
        Option "HorizScrollDelta" "-75"
      '';
    };
  };

  programs.light.enable = true;
  programs.java.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  environment.systemPackages = with pkgs; [
    beets
    calibre
    chromium
    comfortaa
    gimp
    keynav
    kdeconnect
    maim
    mu
    musescore
    oxygenfonts
    redshift
    spotify
    tmux
    vlc
    ympd

    # extra development
    clang
    rr
    shellcheck

    # python development
    # python
    # pythonPackages.importmagic

    # all necessary for emacs build epdfinfo
    autoconf
    automake
    autobuild
    pkgconfig
    gnum4
  ];

  # virtualisation.virtualbox.enableHardening = true;
  virtualisation.virtualbox.host.enable = true;
  services.physlock.enable = true;

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

  # Can't be enabled in virtual guests
  #rngd.enable = true; # feed hardware randomness to kernel when possible

  # Printing
  # services.printing = {
  #   enable  = true;
  #   drivers = [ pkgs.hplip ];
  # };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
