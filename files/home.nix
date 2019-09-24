{ pkgs, ... }:

{
  home.packages = [
  ];

  services.dunst = {
    enable = true;
  };

  programs.home-manager = {
    enable = true;
  };
}
