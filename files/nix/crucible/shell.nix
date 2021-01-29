with import <nixpkgs> { };

mkShell {
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${zlib}/lib
  '';
  buildInputs =  [
    clang_10
    llvm_10
    yices
    z3
    (haskell.packages.ghc884.ghcWithHoogle (hpkgs: with hpkgs; []))
    haskellPackages.haskell-language-server
    haskellPackages.ghcid
    haskellPackages.cabal-install
    zlib
  ];
}
