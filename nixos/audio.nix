{ config, pkgs, ... }:

{
  imports = [];

  environment.systemPackages = with pkgs; [
    pavucontrol
    ncmpcpp
    gst_ffmpeg
    gst_plugins_base
    gst_plugins_good
    gst_plugins_bad
    gst_plugins_ugly
  ];

  fileSystems."/home/siddharthist/library" =
    { device = "/dev/disk/by-uuid/3babbf74-417f-45a9-876b-e8f6ef880798";
      options = [ "defaults" "rw" "user" "noauto" ];
    };

  # fileSystems."/var/lib/mopidy" =
  #   { device = "/dev/disk/by-uuid/3babbf74-417f-45a9-876b-e8f6ef880798";
  #     options = [ "defaults" "rw" "user" "noauto" ];
  #   };

  # services.mopidy = {
  #   enable = true;
  #   configuration = ''
  #     [local]
  #     media_dir = /var/lib/mopidy/music
  #     scan_follow_symlinks = true

  #     [file]
  #     enabled = true
  #     media_dirs = /var/lib/mopidy/music
  #     follow_symlinks = true

  #     [soundcloud]
  #     auth_token = 1-35204-13055450-d7e88a776b7aaf8
  #   '';
  #   extensionPackages = with pkgs; [
  #     # TODO: upstream mopidy-podcast
  #     mopidy-musicbox-webclient
  #     mopidy-soundcloud
  #     mopidy-youtube
  #   ];
  # };

  #services.ympd.enable = true;

  services.mpd = {
    enable = true;
    user = "siddharthist";
    group = "siddharthist";
    musicDirectory = "/home/siddharthist/library/music-beets";
    dataDir = "/home/siddharthist/library/mpd-data";
    # use aplay -l to find hw:card,device tuple
    extraConfig = ''
      audio_output {
        type "alsa"
        name "usb sound card"
        device "hw:2,0"
      }
    '';
  };

  # sound.enableMediaKeys = true; # sxhkd takes care of this
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
}
