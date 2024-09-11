# -*- mode: nix -*-
{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
    ../general.nix

    ../audio.nix
    ../networking.nix
    ../i3.nix
  ];

  environment.systemPackages = with pkgs; [
    acpi # battery monitoring in scripts
  ];

  programs.light.enable = true;

 services.xserver = {
    enable = true;
    xkb.layout = "us";

    monitorSection = ''
      DisplaySize   294 166
    '';

    synaptics = {
      enable = false;  # TODO?
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

  # systemd.user =
  #   let variables = import ../hosts/this/variables.nix;
  #   in {
  #     # TODO lock down
  #     services = {
  #       calendar = {
  #         description = "Log the Google Calendar to disk every once in a while";
  #         serviceConfig = {
  #           Type = "oneshot";
  #           ExecStart = ''
  #             ${pkgs.bash}/bin/bash -c "${pkgs.gcalcli}/bin/gcalcli agenda > /home/${variables.username}/.local/share/calendar.txt"
  #           '';
  #         };
  #       };
  #     };
  #     timers = {
  #       calendar = {
  #         enable = true;
  #         description = "Log the Google Calendar to disk every once in a while";
  #         wantedBy = [ "timers.target" ];
  #         timerConfig = {
  #           OnCalendar = "*:0/15";  # every 15 minutes
  #         };
  #       };
  #     };
  #   };

  # KMonad: https://github.com/david-janssen/kmonad#uinput-permissions
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';
  boot.kernelModules = [ "uinput" ];
}
