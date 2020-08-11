# A systemd serviceConfig attribute blob with defaults for graphical services.
#
{ config
, userService ? false
}:

(import ./service.nix { inherit userService; }) // {
  # TODO
  # environment.DISPLAY = ":${builtins.toString config.services.xserver.display}";
  after = [ "display-manager.service" ];
  partOf = [ "display-manager.service" ];
  wantedBy = [ "graphical.target" ];
}
