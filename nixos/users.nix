{ config, pkgs, ... }:

{

  users = {

    groups = {
      langston = {
        gid = 1000;
      };
      uinput = {};
    };

    # Remember to set the password with `passwd`
    users = {

      # Personal
      langston = {
        isNormalUser = true;
        home = "/home/langston";
        shell = pkgs.zsh;
        createHome = true;
        description = "Langston Barrett";
        uid = 1000;
        group = "langston";
        openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBxEkxXoFPhuo8l3A+OpRGjbaXrdUFriIU71XjY7U9Ry langston.barrett@gmail.com"];
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
