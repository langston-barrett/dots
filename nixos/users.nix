{ config, pkgs, ... }:

let variables = import ./hosts/this/variables.nix;
in {

  users = {

    groups = {
      "${variables.username}" = {
        gid = 1000;
      };
      uinput = {};
    };

    # Remember to set the password with `passwd`
    users = {

      # Personal
      "${variables.username}" = {
        isNormalUser = true;
        home = "/home/${variables.username}";
        shell = pkgs.zsh;
        createHome = true;
        description = "Langston Barrett";
        uid = 1000;
        group = "${variables.username}";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBxEkxXoFPhuo8l3A+OpRGjbaXrdUFriIU71XjY7U9Ry"
        ];
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
