{ config, pkgs, ... }:

{
  imports = [];

  programs.ssh = {
    startAgent = true;
    agentTimeout = null; # forever
    extraConfig = ''
      Host cfern
        HostName ec2-54-188-130-129.us-west-2.compute.amazonaws.com
        Port 22
        User ubuntu
    '';
  };
}
