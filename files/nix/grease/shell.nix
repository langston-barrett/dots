{ ghc ? "ghc96"
, unstableHaskell ? true
}:

let
  pkgs = import <nixpkgs> { };
  unstable = import <unstable> { };
  hPkgs = if unstableHaskell then unstable else pkgs;
in pkgs.mkShell {
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.zlib}/lib
  '';
  buildInputs =  [
    pkgs.clang_11
    pkgs.llvm_11
    pkgs.cvc5
    pkgs.yices
    pkgs.z3
    pkgs.mdbook

    hPkgs.haskell.packages.${ghc}.ghc
    hPkgs.haskell.packages.${ghc}.haskell-language-server
    hPkgs.haskellPackages.apply-refact
    hPkgs.haskellPackages.ghcid
    hPkgs.haskellPackages.hlint
    hPkgs.haskellPackages.cabal-install
    pkgs.zlib  # needed for Haskell
  ];
}
