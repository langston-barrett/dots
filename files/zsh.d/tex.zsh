#!/usr/bin/env zsh

# Delete all files with the given extensions (list as string, separated by " ")
function remove_exts() {
  setopt shwordsplit # Split on " "
  # Group with or: Start with \(, Separate exts with \|, end with \)
  re='^.+\.\('; for ext in $1; do re+=$ext && re+='\|'; done; re+='\)$'
  # Now, find and delete based on that regexp
  find . -iregex "$re" -exec rm {} +
  unsetopt shwordsplit
}
# Description: delete all tex compilation extras in the current directory
function cleantex() { remove_exts "toc log fls fdb bib fdb_latexmk out aux synctex.gz" }
