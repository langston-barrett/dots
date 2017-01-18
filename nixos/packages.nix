{ config, pkgs, ... }:

{
  # This includes packages we don't always need, but we'd hate to rebuild. These
  # are marked by #.
  environment.systemPackages = with pkgs; [
    R #
    blueman #
    curl
    emacs
    file
    firefox
    gcc #
    gdb #
    ghc #
    git
    gnupg
    google-chrome #
    imagemagick
    mpw
    su
    unzip
    vim
    zip

    texlive.combined.scheme-full # lualatex, etc.
    zathura
  ];
}
