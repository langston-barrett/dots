{ pkgs, ... }:

let
  myEmacs =
    with pkgs; ((emacsPackagesNgGen emacsGit).emacsWithPackages (epkgs: [
      #epkgs.emacsql-sqlite
      epkgs.exwm
      epkgs.vterm
      epkgs.pdf-tools
    ])).overrideAttrs (attrs: {
      propagatedBuildInputs = (attrs.propagatedBuildInputs or []) ++ [pkgs.binutils pkgs.gcc];
    });
in {
  home.packages = with pkgs; [
    fasd
  ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  manual.manpages.enable = true;

  xsession = {
    enable = false;
    windowManager.command = ''
      pkill .emacs
      ${myEmacs}/bin/emacs --daemon -f exwm-enable &> /tmp/emacs || i3
      exec ${myEmacs}/bin/emacsclient -c &> /tmp/emacs || i3
    '';
  };

  services = {
    dunst = {
      enable = false;
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

    emacs.enable = true;

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

    # password-store = {
    #   enable = true;
    # };

    emacs = {
      enable = true;
      package = myEmacs;
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
        EDITOR = "vi";
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
      timers = {
      };

      services.org-server = {
        Service = {
          WorkingDirectory = "/home/langston/org/meta/export/all";
          ExecStart = ''
            ${pkgs.python3}/bin/python3 -m http.server 9008
          '';
        };
      };

      services.herms-server = {
        Service = {
          WorkingDirectory = "/home/langston/org/meta/export/herms";
          ExecStart = ''
            ${pkgs.caddy}/bin/caddy file-server -listen 0.0.0.0:9002
          '';
        };
      };
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
