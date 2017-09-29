#!/usr/bin/env zsh

# This is deprecated in favor of latexmk.
# Description: delete all tex compilation extras in the current directory
function cleantex() { echo "running latexmk -c"; latexmk -c > /dev/null }
