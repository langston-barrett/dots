# This file contains a minimal set of packages for a development environment.

{ pkgs ? import <nixpkgs> { }
, doC ? true
, doHaskell ? false
}:

with pkgs; [
  (aspellWithDicts (ds: with ds; [ en ]))
  bat
  curl
  delta
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
  python3
  ripgrep
  tldr
  tree
  trash-cli
  unzip
  wget
  zip
  zsh-completions
] ++ (if ! doC
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
