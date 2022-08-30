{ ghc ? "ghc8107"
, unstableHaskell ? false
}:

let
  pkgs = import <nixpkgs> { };
  unstable = import <unstable> { };
  hPkgs = if unstableHaskell then unstable else pkgs;
in pkgs.mkShell {
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.zlib}/lib:${pkgs.ncurses6}/lib
  '';
  buildInputs =  [
    pkgs.clang_11
    pkgs.llvm_11
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
    unstable.nodePackages.typescript-language-server

    # llvm
    pkgs.cmake
    pkgs.lld
    pkgs.ncurses6
    pkgs.ninja
    pkgs.lit

    # blight
    pkgs.python3
    pkgs.python3Packages.click
    pkgs.python3Packages.pip
    pkgs.python3Packages.pydantic
    pkgs.python3Packages.typing-extensions

    # rust-analyzer, build-bom
    pkgs.rustup
    pkgs.rust-analyzer
  ];
}
