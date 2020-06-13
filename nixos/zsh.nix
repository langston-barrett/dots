{ config, pkgs, ... }:

{
  imports = [];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    #enableSyntaxHighlighting = true;

    # prompt now set in ../files/zsh.d/prompt.zsh
    promptInit = "";

    interactiveShellInit = ../files/zshrc;
  };

  programs.bash = {
    enableCompletion = true;
  };
}
