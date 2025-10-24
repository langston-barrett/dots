#!/usr/bin/env zsh

# https://github.com/Lyndir/MasterPassword/blob/master/platform-independent/cli-c/mpw.bashrc

mpw() {
  export MPW_FULLNAME='Langston Barrett'
  export MPW_SITETYPE=x
  printf %s "$(command mpw -t x "$@")" | copy
}

passmpw () {
  pass -c master
}
