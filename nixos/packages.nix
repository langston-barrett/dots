{ config, pkgs, ... }:

let
  # Dropbox crashed with PaX before this commit
  dropbox_nixpkgs = import (pkgs.fetchFromGitHub {
    owner  = "ttuegel";
    repo   = "nixpkgs";
    # This is the commit that included the dropbox patch
    rev    = "d486fb053b3f148e5989d6cd3e07a69eaf75d0bf";
    sha256 = "14s283bwh77266zslc96gr7zqaijq5b9k4wp272ry27j5q8l3h4i";
  }) {};
in
{
  # Dropbox
  nixpkgs.config.allowUnfree = true;

  # This includes packages we don't always need, but we'd hate to rebuild. These
  # are marked by #.
  environment.systemPackages = with pkgs; [
    # TODO: nixos won't rebuild with this :'(
    #dropbox_nixpkgs.dropbox
    aspell
    atool # "compress" command in ranger
    blueman #
    conky
    curl
    emacs
    exfat
    # fd # "find" replacement
    file
    firefox
    gcc #
    ghc #
    git
    gnupg
    #TODO: this fails to build
    #google-chrome-beta
    imagemagick
    kcolorchooser
    mpw
    p7zip
    ranger
    su
    sudo
    unzip
    vim
    zip

    texlive.combined.scheme-full # lualatex, etc.
    zathura
  ];
}
