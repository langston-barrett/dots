{ ghc ? "ghc901"
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
    pkgs.yices
    pkgs.z3

    (hPkgs.haskell.packages.${ghc}.ghcWithPackages (hpkgs: with hpkgs; []))
    # hPkgs.haskell-language-server
    hPkgs.haskellPackages.ghcid
    hPkgs.haskellPackages.cabal-install
    pkgs.zlib  # needed for Haskell
  ];
}
