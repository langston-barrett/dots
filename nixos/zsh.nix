{ config, pkgs, ... }:

{
  imports = [];

  environment = {

    systemPackages = with pkgs; [
      direnv # automatically invoke/revoke a nix-shell
      fasd
    ];

    shellAliases = {
      amiconnected = "while true; do if curl google.com &> /dev/null; then echo \"$(date +%H:%M) ~~~GOOD~~~\"; else echo \"$(date +%H:%M) ---BAD---\"; fi; sleep 5; done";
      test-ssh = "ssh -T git@github.com";
      ls1 = "ls -1";
      reload = "source /etc/zshrc";
      # TODO: imagemagick
      screenshot = "import -window root ~/Downloads/screenshot.jpg";
      conky = "conky --config=$XDG_CONFIG_HOME/conky/conkyrc";
      nixpkgs-pr-review = "export TRAVIS_BUILD_DIR=$PWD && ./maintainers/scripts/travis-nox-review-pr.sh nixpkgs-verify nixpkgs-manual nixpkgs-tarball && ./maintainers/scripts/travis-nox-review-pr.sh nixos-options nixos-manual";

      # Pipe stuff to this command and get a URL back
      pastebin = "curl -F \"clbin=<-\" https://clbin.com";

      # Docker
      docker-gc = "sudo docker ps -a -q -f status=exited | xargs --no-run-if-empty sudo docker rm";
      docker-gc-images = "docker images -q | xargs --no-run-if-empty docker rmi || true";

      # systemd
      sys   = "sudo systemctl";
      syss  = "sudo systemctl status";
      sysr  = "sudo systemctl restart";
      sysu  = "systemctl --user";
      sysus = "systemctl --user status";
      sysur = "systemctl --user restart";

      # Convenience
      # TODO: nix function
      work-algebra = ''
        cd ~/Dropbox/langston/tex/math332; \
          zathura dummit.djvu & disown; \
          zathura notes/tex/1-ring-theory.pdf & disown; \
          ${pkgs.emacs}/bin/emacsclient --create-frame & disown; \
          sleep 1; exit 0
      '';
    };

    variables = {
      XDG_CONFIG_HOME = "$HOME/.config";
      EDITOR           = "emacs";
      #PATH            = "$PATH:$XDG_CONFIG_HOME/bin";

      # masterpassword
      MPW_FULLNAME = "Langston Barrett";
      MPW_SITETYPE = "x";
    };
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    #enableSyntaxHighlighting = true;

    # prompt now set in ../files/zsh.d/prompt.zsh
    promptInit = "";

    # zshrc
    interactiveShellInit = ''
      # See NixOS/nix#1056
      if [ -n "$IN_NIX_SHELL" ]; then
        export TERMINFO=/run/current-system/sw/share/terminfo

        # Reload terminfo
        real_TERM=$TERM; TERM=xterm; TERM=$real_TERM; unset real_TERM
      fi

      eval "$(fasd --init posix-alias zsh-hook)"
      if [[ -z $IN_NIX_SHELL ]]; then
        eval "$(direnv hook zsh)"
      fi
      source_all() {[[ -d $1 ]] && for f in $1/*.zsh; do source "$f"; done; unset f;}
      source_all $HOME/.zsh.d

      # http://www.zsh.org/mla/users/2007/msg00944.html
      # https://goo.gl/CsT6cQ
      # TMOUT=30
      # TRAPALRM () {
      #   zle reset-prompt
      # }
    '';
  };

  programs.bash = {
    enableCompletion = true;
  };
}
