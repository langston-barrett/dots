{ pkgs ? import <nixpkgs>
, unstable ? import <unstable>
, config ? { allowUnfree = true; }
}:

# nix-shell --pure -p nix-prefetch-git --run "nix-prefetch-git https://github.com/NixOS/nixpkgs.git > json/master.json"
let get = path:
      let
        nixpkgs = builtins.fromJSON (builtins.readFile path);
        src = pkgs.fetchFromGitHub {
          owner = "NixOS";
          repo  = "nixpkgs";
          inherit (nixpkgs) rev sha256;
        };
      in import src { inherit config; };
in {
  "18.03" = get ./json/19.03.json;
  "master" = get ./json/master.json;
  "unstable" = unstable { inherit config; };
}
