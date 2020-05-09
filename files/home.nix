{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fasd
  ];

  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          geometry = "500-30+50";
          transparency = 10;
          frame_color = "#eceff1";
          font = "Hack 14";
          word_wrap = true;
        };
      };
    };

    lorri.enable = true;

    kbfs = {
      enable = true;
    };
    keybase = {
      enable = true;
    };
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  # programs.home-manager = {
  #   enable = true;
  # };

  programs = {
    gpg.enable = true;

    password-store = {
      enable = true;
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      sessionVariables = {
        # masterpassword
        MPW_FULLNAME = "Langston Barrett";
        MPW_SITETYPE = "x";
        XDG_CONFIG_HOME = "$HOME/.config";
        EDITOR = "emacs";
      };
      shellAliases = {
        amiconnected = "while true; do if curl google.com &> /dev/null; then echo \"$(date +%H:%M) ~~~GOOD~~~\"; else echo \"$(date +%H:%M) ---BAD---\"; fi; sleep 5; done";
        test-ssh = "ssh -T git@github.com";
        ls1 = "ls -1";
        screenshot = "import -window root ~/Downloads/screenshot.jpg";
        # Pipe stuff to this command and get a URL back
        pastebin = "curl -F \"clbin=<-\" https://clbin.com";

        # Docker
        docker-gc = "sudo docker ps -a -q -f status=exited | xargs --no-run-if-empty sudo docker rm";
        docker-gc-images = "docker images -q | xargs --no-run-if-empty docker rmi || true";

        # systemd
        top5  = "watch 'ps aux | sort -nrk 3,3 | head -n 5'";
        cdr = "cd $(fd --type d | fzf)";
      };
    };
  };

  systemd = {
    user = {
      timers = {};
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
