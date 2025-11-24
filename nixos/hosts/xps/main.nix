# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    # <nixos-hardware/common/cpu/intel>

    ../../roles/laptop.nix

    # ../../bluetooth.nix
    # ../../steam.nix
  ];

  networking.hostName = "langston-xps";

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };
  boot.initrd.luks.devices."luks-36ee2a1d-ab2b-426b-881c-b8954a62e139".device = "/dev/disk/by-uuid/36ee2a1d-ab2b-426b-881c-b8954a62e139";

  # services.logind.extraConfig = "RuntimeDirectorySize=8G";

  services.mullvad-vpn.enable = true;
  services.mullvad-vpn.package = pkgs.mullvad-vpn;
  networking.nameservers = [ "1.1.1.1#one.one.one.one" "1.0.0.1#one.one.one.one" ];
  services.resolved = {
    enable = true;
    dnssec = "true";
    domains = [ "~." ];
    fallbackDns = [ "1.1.1.1#one.one.one.one" "1.0.0.1#one.one.one.one" ];
    dnsovertls = "true";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "25.05"; # Did you read the comment?
}
