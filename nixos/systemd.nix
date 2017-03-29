{ config, pkgs, ... }:

# These are my hand-made systemd user services that still need to be upstreamed

{
  imports = [];

  environment.systemPackages = with pkgs; [
    feh
    rxvt_unicode-with-plugins
    sxhkd
  ];

  systemd = {
    user = {

      services = {

        #i3lock = {
          #description = "Automatically lock screen before going to sleep";
          #wantedBy = [ "default.target" ];
          #path = with pkgs; [ xss-lock i3lock-fancy ];
          #serviceConfig = {
            #Restart = "always";
            #ExecStart = "${pkgs.xss-lock}/bin/xss-lock ${pkgs.i3lock-fancy}/bin/i3lock-fancy";
          #};
        #};

        feh = {
          enable = true;
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
  };
}
