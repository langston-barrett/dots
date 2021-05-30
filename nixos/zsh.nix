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
        # https://kevin.burke.dev/kevin/profiling-zsh-startup-time/
        if [[ "$PROFILE_ZSH_STARTUP" == true ]]; then
            # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
            PS4=$'%D{%M%S%.} %N:%i> '
            exec 3>&2 2>$HOME/tmp/startlog.$$
            setopt xtrace prompt_subst
        fi

        source_all_sh() {[[ -d $1 ]] && for f in $1/*.sh; do source "$f"; done; unset f;}
        source_all() {[[ -d $1 ]] && for f in $1/*.zsh; do source "$f"; done; unset f;}
        source_all_sh $HOME/.sh.d
        source_all $HOME/.zsh.d

        if [[ "$PROFILE_ZSH_STARTUP" == true ]]; then
            unsetopt xtrace
            exec 2>&3 3>&-
        fi
      '';
    };

    bash = {
      enableCompletion = true;
      interactiveShellInit = ''
        function source_all() { [[ -d $1 ]] && for f in $1/*.sh; do source "$f"; done; unset f; }
        source_all $HOME/.sh.d
      '';
    };
  };
}
