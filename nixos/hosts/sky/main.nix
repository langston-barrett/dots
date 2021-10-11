{ config, pkgs, ... }:

{
  imports = [
      ./hardware-configuration.nix

      ../../dev.nix
      ../../roles/server.nix
  ];

  boot.loader.systemd-boot = {
    enable = true;
  };
  networking.hostName = "langston-nixos"; # Define your hostname.
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  users.users.langston = {
    isNormalUser = true;
    extraGroups = [ "docker" "wheel" ]; # Enable ‘sudo’ for the user.
  };
  environment.systemPackages = with pkgs; [
    git
    vim
  ];
  virtualisation.vmware.guest = {
    enable = true;
    headless = true;
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
