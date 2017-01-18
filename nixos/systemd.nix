{ config, pkgs, ... }:

# These are my hand-made systemd user services that still need to be upstreamed

{
  imports = [];

  environment.systemPackages = with pkgs; [
    conky
    feh
    rxvt_unicode-with-plugins
    sxhkd
  ];

  systemd = {
    user = {

      services = {
        sxhkd = {
          description = "Simple X hotkey daemon";
          # documentation = "man:sxhkd(1)";

          after = [ "display-manager.service" ];
          partOf = [ "display-manager.service" ];
          wantedBy = [ "graphical.target" ];

          serviceConfig = {
            ExecStart = "${pkgs.sxhkd}/bin/sxhkd";
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };

        conky = {
          description = "Advanced, highly configurable system monitor based on torsmo";
          after = [ "display-manager.service" ];
          partOf = [ "display-manager.service" ];
          wantedBy = [ "graphical.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.conky}/bin/conky --config=/home/siddharthist/.config/conky/conkyrc";
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };

        feh = {
          description = "Use feh image viewer to set the wallpaper";
          after = [ "display-manager.service" ];
          partOf = [ "display-manager.service" ];
          wantedBy = [ "graphical.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.feh}/bin/feh --no-fehbg --bg-fill /home/siddharthist/.config/wallpaper.jpg";
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };
      };
    };
    # Actually enable them
    services = {
      conky.enable = true;
      compton.enable = true;
      emacs.enable = false;
      sxhkd.enable = false;
    };
  };
}
