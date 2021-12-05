{ config, pkgs, ... }:

{
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      security = "none";
    };
  };
  services.blueman.enable = true;

  environment.systemPackages = with pkgs; [
    bluez
    blueman
  ];
}
