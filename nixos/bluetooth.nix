{ config, pkgs, ... }:

{
  hardware.bluetooth = {
    enable = false;
    powerOnBoot = false;
  };

  environment.systemPackages = with pkgs; [
    bluez
    blueman
  ];
}
