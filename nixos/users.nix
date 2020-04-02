{ config, pkgs, ... }:

{

  users = {

    groups = {
      siddharthist = {
        gid = 1000;
      };
      uinput = {};
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
        extraGroups = [
          "networkmanager"
          "sway"
          "uinput"
          "video"  # light
          "wheel"
        ];
      };
    };
  };

  # KMonad: https://github.com/david-janssen/kmonad#uinput-permissions
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';
  boot.kernelModules = [ "uinput" ];
}
