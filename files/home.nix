{ pkgs, ... }:
  programs = --exact {
    exact-gpg.enable = false;

 --height=10% --layout=reverse --prompt='>> '    # password-store = {
    };

-exact      enable = true;
      enable = exact-tree;
        global = {
          geometry = "500-30+50";
          frame_color = "#eceff1";
  };
  programs = {
    gpg.enable = true;

 --height=10% --layout=reverse --prompt='>> '    # password-store = {
    };

      enable = true;
      keybase = {
        enable = true;
      };
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
    #   enable = true;
  };

    user = {
      timers = { };
    };
  };
}
assword-store = {
    #   enable = true;
  };

    user = {
      timers = { };
    };
  };
}

    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
  programs = {
    gpg.enable = true;

    # password-store = {
    #   enable = true;
  };

    user = {
      timers = { };
    };
  };
}
