# To use: nix-shell --run zsh
with import <nixpkgs> {}; stdenv.mkDerivation {
  name = "dots";
  buildInputs = [
    ansible
    shellcheck
  ];
}
