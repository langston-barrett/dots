{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    neomutt
    urlview
  ];
}
