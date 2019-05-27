{ config, pkgs, ... }:

{
  hardware.pulseaudio.support32Bit = true;
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  environment.systemPackages = with pkgs; [
    steam
    # TODO
    #xhost # for running `xhost +` when steam doesn't work

    # see also x.nix#videoDrivers
    # are these necessary?
    mesa
    mesa_drivers
  ];
}
