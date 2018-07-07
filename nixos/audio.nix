{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    flac
    pavucontrol
    ncmpcpp
    gst_ffmpeg
    gst_plugins_base
    gst_plugins_good
    gst_plugins_bad
    gst_plugins_ugly
  ];

  services.mopidy = {
    enable = false;
    # user = "siddharthist";
    # group = "siddharthist";
    configuration = ''
      [local]
      media_dir = /home/siddharthist/Dropbox/langston/music
      scan_follow_symlinks = true

      [file]
      enabled = true
      media_dir = /home/siddharthist/Dropbox/langston/music
      follow_symlinks = true

      [soundcloud]
      auth_token = 1-35204-13055450-d7e88a776b7aaf8
    '';
    extensionPackages = with pkgs; [
      # TODO: upstream mopidy-podcast
      mopidy-moped
      mopidy-musicbox-webclient
      mopidy-soundcloud
      mopidy-youtube
    ];
  };

  services.ympd = {
    enable = false;
    mpd.port = 6700;
    webPort = "6601";
  };

  services.mpd = {
    enable = false;
    user = "siddharthist";
    group = "siddharthist";
    musicDirectory = "/home/siddharthist/Dropbox/langston/music";
    dataDir = "/home/siddharthist/Dropbox/langston/archive/backup/mpd";

    network.port = 6700;

    # use aplay -l to find hw:card,device tuple
    extraConfig = ''
      audio_output {
        type "alsa"
        name "usb sound card"
        device "hw:0,0"
      }
    '';
  };

  # sound.enableMediaKeys = true; # sxhkd takes care of this
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
}
