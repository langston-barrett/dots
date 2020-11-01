{ config, pkgs, ... }:

{
  programs.steam.enable = true;

  # https://support.steampowered.com/kb_article.php
  # UDP remote port 27000--27100: Game traffic
  # UDP local port 27031-27036: Remote Play
  # TCP local port 27036: Remote Play
  # UDP remote port 4380
  networking = {
    firewall = {
      allowedUDPPortRanges = [
        { from = 27000; to = 27100; }
        { from = 27031; to = 27036; }
      ];
      allowedUDPPorts = [ 4380 ];
      allowedTCPPortRanges = [ ];
      allowedTCPPorts = [ 27036 ];
    };
  };
}
