{ config, pkgs, ... }:

let variables = import ./hosts/this/variables.nix;
in {
  users = {
    groups = {
      "${variables.username}" = {
        gid = 1000;
      };
      "mar" = {
        gid = 1001;
      };
      uinput = {};
    };


    # Remember to set the password with `passwd`
    users = {
      "${variables.username}" = {
        isNormalUser = true;
        home = "/home/${variables.username}";
        shell = pkgs.zsh;
        createHome = true;
        uid = 1000;
        group = "${variables.username}";
        openssh.authorizedKeys.keys = [ ];
        extraGroups = [
          "networkmanager"
          "uinput"
          "video"  # light
          "wheel"
        ];
      };
    };
  };
}
