# dots.nix
# https://nixos.org/wiki/Development_Environments

let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  dots = stdenv.mkDerivation rec {
    name = "dots";
    version = "0";
    src = ./.;
    buildInputs = with pkgs; [
      python27Packages.ansible2
    ];
  };
}
