{ ghc ? "ghc924"
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
    pkgs.clang_8
    pkgs.llvm_8
    pkgs.cvc5
    pkgs.yices
    pkgs.z3

    (hPkgs.haskell.packages.${ghc}.ghcWithHoogle (hpkgs: with hpkgs; []))
    hPkgs.haskell-language-server
    hPkgs.haskellPackages.ghcid
    hPkgs.haskellPackages.hlint
    hPkgs.haskellPackages.cabal-install
    pkgs.zlib  # needed for Haskell
  ];
}
