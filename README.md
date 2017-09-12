# dots
[![Build Status](https://travis-ci.org/siddharthist/dots.svg?branch=master)](https://travis-ci.org/siddharthist/dots)

These are not your everyday dotfiles! This repo hosts both per-system
configuration via NixOS and per-user configuration via Ansible.

Ansible roles aren't used to enforce minimal complexity. Ansible is only used
for symlinking and mild templating.

Anything complex or involving services/packages is upstreamed to nixpkgs.

## Wallpapers

I used the following commands to create a 3200x1800 wallpaper with the Nix
snowflake logo:
```
convert -size 3200x1800 xc:#F6F6F6 ~/.config/wallpaper-blank.png
inkscape -z -e nix-snowflake.png -w 1024 -h 1024 nix-snowflake.svg
convert nix-snowflake.png \
        -alpha set -channel RGBA -fill none -opaque white \
        nix-snowflake-transparent.png
```
where `nix-snowflake.svg` is taken
from [NixOS Artwork](https://github.com/NixOS/nixos-artwork).
