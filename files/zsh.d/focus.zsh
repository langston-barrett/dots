#!/usr/bin/env zsh

set_focus() {
  echo "$1" > /run/user/1000/focus.txt
}
