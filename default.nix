# To use: nix-shell --run 'exec zsh'
with import <nixpkgs> {}; mkShell {
  name = "dots";
  buildInputs = [ shellcheck ];
}
