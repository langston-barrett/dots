#!/usr/bin/env bash

# https://github.com/Aloxaf/fzf-tab/wiki/Preview#show-file-contents

if [[ -d "${1}" ]]; then
  ls --color=always "${1}" | bat --plain --color=always
elif [[ $(file --brief --dereference "${1}") == "ASCII text" ]]; then
  bat --plain --color=always "${1}"
elif [[ -f "${1}" ]]; then
  lesspipe.sh "${1}" | bat --plain --color=always
else
  exit 0
fi
