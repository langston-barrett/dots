{ config, pkgs, ... }:

{
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      promptInit = "";
      interactiveShellInit = "source ~/.config/zsh/init.zsh";
    };

    bash = {
      enableCompletion = true;
      interactiveShellInit = "source ~/.config/bash/init.sh";
    };
  };
}
