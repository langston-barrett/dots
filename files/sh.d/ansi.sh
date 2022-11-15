#!/bin/sh

ansi_block_cursor() { printf '\e[1 q'; }
ansi_bar_cursor() { printf '\e[5 q'; }
ansi_no_cursor_blink() {  printf '\033[?12l'; }
