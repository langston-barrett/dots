{ config, pkgs, ... }:

{
  # List services that you want to enable:
  security.sudo.enable = true;

  users = {

    #defaultUserShell = "/nix/var/nix/profiles/default/bin/zsh";

    groups = {
      siddharthist = {
        gid = 1000;
      };
    };

    # Remember to set the password with `passwd`
    users = {
      siddharthist = {
        isNormalUser = true;
        home = "/home/siddharthist";
        createHome = true;
        description = "Langston Barrett";
        uid = 1000;
        group = "siddharthist";
        extraGroups = [ "wheel" "networkmanager" ];
        #shell = "/run/current-system/sw/bin/zsh";
      };
    };
  };
}
