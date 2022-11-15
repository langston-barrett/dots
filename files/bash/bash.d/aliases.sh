#!/usr/bin/env bash

# Expand aliases with SPC
bind -m vi-insert "\C-o:magic-space"
bind -m vi-insert "\C-p:alias-expand-line"
bind -m vi-insert '" ":"\C-o\C-p"'
