{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    neomutt
    offlineimap
    sqlite # offlineimap
    urlview
  ];

  services.offlineimap = {
    enable  = true;
    install = true;
    # every ten minutes
    onCalendar = "*:0/10";
  };
}
