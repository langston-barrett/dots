{ config, pkgs, ... }:

{
  # List services that you want to enable:
  security.sudo.enable = true;

  users = {

    groups = {
      siddharthist = {
        gid = 1000;
      };
    };

    # Remember to set the password with `passwd`
    users = {

      # Personal
      siddharthist = {
        isNormalUser = true;
        home = "/home/siddharthist";
        shell = pkgs.zsh;
        createHome = true;
        description = "Langston Barrett";
        uid = 1000;
        group = "siddharthist";
        extraGroups = [ "networkmanager" "sway" "wheel" ];
      };
    };
  };
}
