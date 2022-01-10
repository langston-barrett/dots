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
    pkgs.clang_8
    pkgs.llvm_8
    pkgs.yices
    pkgs.z3

    pkgs.zlib  # needed for Haskell
    (hPkgs.haskell.packages.${ghc}.ghcWithHoogle (hpkgs: with hpkgs; []))
    hPkgs.haskellPackages.ghcid
    hPkgs.haskellPackages.cabal-install

    hPkgs.haskell-language-server
    hPkgs.haskellPackages.hlint
    # hPkgs.haskellPackages.ormolu

    # vscode-ui
    unstable.nodejs_latest
    unstable.vscode
  ];
}
