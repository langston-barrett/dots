{ config, pkgs, ... }:

{
  # This includes packages we don't always need, but we'd hate to rebuild. These
  # are marked by #.
  environment.systemPackages = with pkgs; [
    blueman #
    curl
    emacs
    firefox
    file
    gcc #
    gdb #
    ghc #
    git
    gnupg
    google-chrome
    mpw
    R #
    su
    vim
    unzip
    zip

    texlive.combined.scheme-full # lualatex, etc.
    zathura

    haskellPackages.Agda
  ];
}
