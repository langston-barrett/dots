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
      # "mar" = {
      #   isNormalUser = true;
      #   home = "/home/mar";
      #   shell = pkgs.zsh;
      #   createHome = true;
      #   group = "mar";
      #   uid = 1001;
      #   openssh.authorizedKeys.keys = [
      #     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE5O29jTgOIJDHnqcWcDK3MYPmocQV6ElcCwmNOZrdLA marborrego@marborrego-ThinkPad-T440s"
      #   ];
      # };
      "${variables.username}" = {
        isNormalUser = true;
        home = "/home/${variables.username}";
        shell = pkgs.zsh;
        createHome = true;
        uid = 1000;
        group = "${variables.username}";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBxEkxXoFPhuo8l3A+OpRGjbaXrdUFriIU71XjY7U9Ry"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKAzKzNdP1IRtpGMvEl2vgcSRSNBUw6bqxDoLLUcdhrY phone"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICAVTLXwaK6r+lfbDQONTYbJmayYBg+AAqxEa4hWAf8K laptop"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHKfYV86oCrLUKZqjB7H1tlCfY/qDgvvn3oYq/snjx/d langston@pop-os"
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
}
