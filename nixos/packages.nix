{ config, pkgs, ... }:

# let
#   eighteen03 = (import ./pinned-pkgs.nix { pkgs = pkgs; }).eighteen03;
# in
{
  # Dropbox
  nixpkgs.config.allowUnfree = true;

  # This includes packages we don't always need, but we'd hate to rebuild. These
  # are marked by #.
  environment.systemPackages = with pkgs; [
    # TODO: nixos won't rebuild with this :'(
    aspell
    atool # "compress" command in ranger
    blueman #
    dropbox
    conky
    curl
    emacs
    exfat
    # fd # "find" replacement
    file
    gcc #
    # Try: nix-env -qaA nixos.haskellPackages
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      cabal-install
      hoogle
      HUnit
      QuickCheck
      tasty
      tasty-hunit
      tasty-quickcheck
      text
    ])) #
    git
    gnupg
    google-chrome-beta
    imagemagick
    mpw
    p7zip
    ranger
    su
    sudo
    unzip
    zip

    texlive.combined.scheme-full # lualatex, etc.

    # Graphical
    firefox
    kcolorchooser
    ksshaskpass # for mpw
    zotero
    xpdf
    zathura
  ];
}
