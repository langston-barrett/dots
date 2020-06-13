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
  fasd
  fd
  file
  fzf
  git
  gnumake
  htop
  jq
  manpages
  mpw
  nix-prefetch-git
  pet
  python3
  ranger
  silver-searcher # ag
  tldr
  trash-cli
  unzip
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
