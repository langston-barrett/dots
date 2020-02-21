{ config, pkgs, ... }:

{
  imports = [];

  programs.ssh = {
    startAgent = true;
    agentTimeout = null; # forever
    extraConfig = ''
    '';
  };
}
