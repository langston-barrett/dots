#!/usr/bin/env zsh

# https://github.com/Lyndir/MasterPassword/blob/master/platform-independent/cli-c/mpw.bashrc

mpw() {
  export MPW_FULLNAME='Langston Barrett'
  export MPW_SITETYPE=x
  _copy() {
    if hash pbcopy 2>/dev/null; then
      pbcopy
    elif hash xclip 2>/dev/null; then
      xclip -selection clip
    elif hash xsel 2>/dev/null; then
      xsel -ib
    else
      cat; echo 2>/dev/null
      return
    fi
    echo >&2 "Copied!"
  }

  # Empty the clipboard
  :| _copy 2>/dev/null

  # Start Master Password and copy the output.
  printf %s "$(command mpw -t x "$@")" | _copy
}

passmpw () {
  pass -c master
}
