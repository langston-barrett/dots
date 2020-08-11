{ config, pkgs, ... }:

let variables = import ./hosts/this/variables.nix;
    mpdOrMopidy = "mpd";
    mpdPort = 6600;
    ympdPort = 6601;
    musicDir = "/home/${variables.username}/Dropbox/langston/music/music";
in {
  imports = [ ./services/ympd.nix ];

  environment.systemPackages = with pkgs; [
    flac
    ncmpcpp
    # gst_ffmpeg
    gst_plugins_base
    gst_plugins_good
    gst_plugins_bad
    gst_plugins_ugly
  ] ++ (if mpdOrMopidy == "mopidy" then [ mopidy ] else []);

  # TODO only if using mopidy
  # For file access. Remember to chmod 0750 all directories in the musicDir
  # chain.
  # users.users.mopidy = {
  #   extraGroups = [ "audio" variables.username ];
  # };

  # TODO improve, lock down
  # You'll also want to run `sys start mopidy-scan.service`
  services.mopidy = {
    enable = if mpdOrMopidy == "mopidy" then true else false;
    dataDir = "/home/mopidy/";
    configuration = ''
      [local]
      media_dir = ${musicDir}
      scan_follow_symlinks = true

      [file]
      enabled = true
      media_dirs = ${musicDir}
      follow_symlinks = true

      [soundcloud]
      auth_token = 1-35204-13055450-d7e88a776b7aaf8

      [mpd]
      enabled = true
      hostname = 127.0.0.1
      port = ${builtins.toString mpdPort}
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

  services.myympd = {
    enable = true;
    webPort = builtins.toString ympdPort;
  };

  services.mpd = {
    enable = if mpdOrMopidy == "mpd" then true else false;
    user = variables.username;
    group = variables.username;
    musicDirectory = musicDir;
    dataDir = "/home/${variables.username}/Dropbox/langston/archive/backup/mpd";
    network.port = mpdPort;

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
}
