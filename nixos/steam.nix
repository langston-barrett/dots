{ config, pkgs, ... }:

{
  hardware.pulseaudio.support32Bit = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # https://support.steampowered.com/kb_article.php 
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

    # UDP remote port 27000--27100: Game traffic
    # UDP local port 27031-27036: Remote Play
    # TCP local port 27036: Remote Play
    # UDP remote port 4380

  environment.systemPackages = with pkgs; [
    steam
    # TODO
    #xhost # for running `xhost +` when steam doesn't work
  ];
}
