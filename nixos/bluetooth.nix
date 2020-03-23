{ config, pkgs, ... }:

{
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  environment.systemPackages = with pkgs; [
    bluez
    blueman
  ];
}
