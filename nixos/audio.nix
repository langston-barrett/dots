{ config, pkgs, ... }:

let
  mpd_mopidy  = "mopidy";
  mpd_port    = 6600;
  music_dir   = "/home/siddharthist/music";
in {
  environment.systemPackages = with pkgs; [
    flac
    pavucontrol
    ncmpcpp
    # gst_ffmpeg
    gst_plugins_base
    gst_plugins_good
    gst_plugins_bad
    gst_plugins_ugly
  ] ++ (if mpd_mopidy == "mopidy" then [ mopidy ] else []);

  # For file access. Remember to chmod 0750 all directories in the music_dir
  # chain.
  users.users.mopidy = {
    extraGroups = [ "audio" "siddharthist" ];
  };

  # You'll also want to run `sys start mopidy-scan.service`
  services.mopidy = {
    enable = if mpd_mopidy == "mopidy" then true else false;
    dataDir = "/home/mopidy/";
    configuration = ''
      [local]
      media_dir = ${music_dir}
      scan_follow_symlinks = true

      [file]
      enabled = true
      media_dirs = ${music_dir}
      follow_symlinks = true

      [soundcloud]
      auth_token = 1-35204-13055450-d7e88a776b7aaf8

      [mpd]
      enabled = true
      hostname = 127.0.0.1
      port = ${builtins.toString mpd_port}
      max_connections = 5
      connection_timeout = 60
      zeroconf = Mopidy MPD server on $hostname
      command_blacklist = listall,listallinfo
      default_playlist_scheme = m3u
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
    enable = true;
    mpd.port = mpd_port;
    webPort = "6601";
  };

  services.mpd = {
    enable = if mpd_mopidy == "mpd" then true else false;
    user = "siddharthist";
    group = "siddharthist";
    musicDirectory = music_dir;
    dataDir = "/home/siddharthist/Dropbox/langston/archive/backup/mpd";

    network.port = mpd_port;

    # use this to find hw:card,device tuple:
    # nix-shell -p alsaUtils --pure --run "aplay -l"
    # extraConfig = ''
    #   audio_output {
    #     type "alsa"
    #     name "usb sound card"
    #     device "hw:1,0"
    #   }
    # '';
  };

  # sound.enableMediaKeys = true; # sxhkd takes care of this
  # see https://github.com/NixOS/nixpkgs/issues/39635
  # TODO: see logs
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    # tcp = {
    #   enable = true;
    #   anonymousClients.allowedIpRanges = [ "127.0.0.1" ];
    # };
    # configFile = pkgs.writeText "default.pa" ''
    #   load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
    # '';
    systemWide = true;
  };
}
