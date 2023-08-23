#!/usr/bin/env nix-shell
#!nix-shell -i bash -p haskellPackages.cabal-install -p git -p mdbook
set -ex
rm -f conf.gen.toml
echo "# see conf.sh" > conf.gen.toml
for tool in cabal cargo gh git glab mdbook; do
  cargo run -qr -- -vvvv zle extract $tool $tool.toml > $tool.gen.toml || exit 1
  cat $tool.gen.toml >> conf.gen.toml
done
cp conf.gen.toml conf.toml
