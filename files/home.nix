{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fasd
  ];

  nixpkgs.overlays = [ ];

  manual.manpages.enable = true;

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

    # password-store = {
    #   enable = true;
    # };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    emacs = {
      enable = true;
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = false;
      enableSyntaxHighlighting = true;
      sessionVariables = {
        # masterpassword
        MPW_FULLNAME = "Langston Barrett";
        MPW_SITETYPE = "x";
        XDG_CONFIG_HOME = "$HOME/.config";
        EDITOR = "hx";
      };
    };
  };

  systemd = {
    user = {
      timers = { };
    };
  };
}
