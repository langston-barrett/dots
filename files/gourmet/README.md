# Gourmet

Couldn't figure out how to make gourmet build right, so here's the steps:
 1. Download gourmet source
 2. Copy `gourmet.nix`, `elib_intl.nix` to source folder
 3. `nix-build`
 4. `cd bin; ../result/bin/gourmet & disown`
