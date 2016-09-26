#!/usr/bin/env zsh

# Description: delete all tex compilation extras in the current directory
function cleantex() { remove_exts "toc log fls fdb bib fbd_latexmk out aux"; }
