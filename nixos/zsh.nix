{ config, pkgs, ... }:

{
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      #enableSyntaxHighlighting = true;

      # prompt now set in ../files/zsh.d/prompt.zsh
      promptInit = "";

      interactiveShellInit = ''
        # TODO: Duplicate of files/zshrc
        # See NixOS/nix#1056

        installed() { command -v "$1" >/dev/null 2>&1; }
        if ! installed nix; then
          . "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi

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
      '';
    };

    bash = {
      enableCompletion = true;
    };
  };
}
