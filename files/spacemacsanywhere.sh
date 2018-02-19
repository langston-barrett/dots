#!/usr/bin/env bash
# Inspired by https://github.com/cknadler/vim-anywhere

file=/tmp/spacemacsanywhere
rm -f "$file"
touch "$file"
chmod 0600 "$file" # Make file only readable by you

emacsclient --create-frame $file
cat "$file" | xsel -ib
