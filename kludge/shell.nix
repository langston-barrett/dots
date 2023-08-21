{ pkgs ? import <nixpkgs> { }
, unstable ? import <unstable> { }
}:

pkgs.mkShell {
  buildInputs = [
  ];

  nativeBuildInputs = [
    pkgs.rust-analyzer
    pkgs.rustup
  ];
}
