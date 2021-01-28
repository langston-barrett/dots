with import <nixpkgs> { };

mkShell {
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${zlib}/lib
  '';
  buildInputs =  [
    (haskell.packages.ghc884.ghcWithHoogle (hpkgs: with hpkgs; []))
    haskellPackages.haskell-language-server
    haskellPackages.ghcid
    haskellPackages.cabal-install
    zlib
  ];
}
