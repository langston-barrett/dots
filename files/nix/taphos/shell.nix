{ ghc ? "ghc8104"
, unstableHaskell ? false
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
    pkgs.clang_10
    pkgs.llvm_10
    # pkgs.yices
    # pkgs.z3

    (hPkgs.haskell.packages.${ghc}.ghcWithHoogle (hpkgs: with hpkgs; []))
    # hPkgs.haskell-language-server
    # hPkgs.haskellPackages.ormolu
    hPkgs.haskellPackages.ghcid
    hPkgs.haskellPackages.hlint
    hPkgs.haskellPackages.cabal-install
    pkgs.zlib  # needed for Haskell
  ];
}
