{ config, pkgs, ... }:

{
  imports = [];

  programs.ssh = {
    startAgent = true;
    agentTimeout = null; # forever
    extraConfig = ''
      Host jump-eu
          HostName 173.39.246.71
          IdentityFile ~/.ssh/id_ed25519
          Port 2222
          User lbarrett

      Host tx3
          HostName 128.107.4.225
          IdentityFile ~/.ssh/id_ed25519
          Port 2222
          User lbarrett

      Host london
          HostName 173.39.251.71
          IdentityFile ~/.ssh/id_ed25519
          Port 2222
          User lbarrett
    '';
  };
}
