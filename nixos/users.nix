# Edit this configuration fible to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [];

  environment.systemPackages = with pkgs; [
    sudo
  ];

  # List services that you want to enable:
  security.sudo = {
    enable = true;
    extraConfig = ''
      Cmnd_Alias SUSPEND = /run/current-system/sw/bin/systemctl suspend
      Cmnd_Alias SHUTDOWN = /run/current-system/sw/bin/systemctl poweroff
      Cmnd_Alias REBOOT = /run/current-system/sw/bin/systemctl reboot
      Cmnd_Alias STATUS = /run/current-system/sw/bin/systemctl status
      Cmnd_Alias RFKILL = /run/current-system/sw/bin/rfkill unblock
      Cmnd_Alias WPA_SUPPLICANT = /run/current-system/sw/bin/systemctl restart wpa_supplicant.service
      siddharthist ALL=NOPASSWD: SUSPEND, SHUTDOWN, REBOOT, STATUS, RFKILL, WPA_SUPPLICANT
    '';
  };

  users = {
    defaultUserShell = "/run/current-system/sw/bin/zsh";
    groups = {
      siddharthist = {
        gid = 1000;
      };
    };
    users = {
      siddharthist = {
        isNormalUser = true;
        home = "/home/siddharthist";
        createHome = true;
        description = "Langston Barrett <langston dot barrett at gmail dot com>";
        uid = 1000;
        group = "siddharthist";
        extraGroups = [ "wheel" "networkmanager" ];
        shell = "/run/current-system/sw/bin/zsh";
      };
    };
  };
}
