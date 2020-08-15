with import <nixpkgs> { };
let this = haskellPackages.callPackage ./default.nix { };
in mkShell {
  buildInputs =  [
    # You can specify a GHC version like so:
    # haskell.packages.ghc843.ghcWithHoogle
    (haskell.packages.ghc865.ghcWithHoogle (hpkgs: with hpkgs; [
    ] ++ this.buildInputs ++ this.propagatedBuildInputs))

    haskellPackages.hpack
    haskellPackages.ghcid
    haskellPackages.cabal-install

    # General development
    emacs
    git
    zsh
  ];
}
