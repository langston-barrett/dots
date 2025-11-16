# This file contains a minimal set of packages for a development environment.

{ pkgs ? import <nixpkgs> { }
, unstable ? import <unstable> { }
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
  mypy
  nix-prefetch-git
  python3
  ripgrep
  ruff
  tealdeer # tldr
  tree
  trashy
  unzip
  wget
  zip
  zizmor
  zoxide
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
