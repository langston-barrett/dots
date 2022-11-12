# This file contains a minimal set of packages for a development environment.

{ pkgs ? import <nixpkgs> { }
, doC ? true
, doHaskell ? false
}:

with pkgs; [
  (aspellWithDicts (ds: with ds; [ en ]))
  atool # "compress" command in ranger
  curl
  direnv
  entr
  fasd
  fd
  file
  fzf
  git
  gnumake
  htop
  jq
  lesspipe
  man-pages
  mpw
  nix-prefetch-git
  pet
  python3
  ranger
  silver-searcher # ag
  tldr
  tree
  trash-cli
  unzip
  wget
  zip
  zsh-completions
] ++ lib.optional (pkgs ? "bat") pkgs.bat # only in newer nixos
  ++ (if ! doC
     then []
     else [ gcc
            gdb
          ])
  ++ (if ! doHaskell
     then []
     else [ haskell.compiler.ghc865
            haskellPackages.cabal-install
            haskellPackages.ghcid
            zlib
          ])
