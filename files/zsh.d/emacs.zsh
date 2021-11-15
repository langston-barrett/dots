#!/usr/bin/env bash

alias macs='emacs -q --eval "(load-file \"~/.config/emacs/init.el\")"'
alias cmacs='emacsclient --create-frame --socket-name=/run/user/1000/macs/server'
# alias e="cmacs || macs"
alias macsterm='emacs -q --eval "(load-file \"~/.config/emacs/init.el\")" --eval "(setq term-only t)"'
export PATH=$PATH:$HOME/.config/doom-emacs.d/bin
export DOOMDIR=$HOME/.config/doom
