#!/usr/bin/env zsh

# http://stratus3d.com/blog/2017/10/26/better-vi-mode-in-zshell/

# Unbind all sequences starting with ESC
#
# See man zshzle.
#
# Unfortunately, unbinds arrow keys...
#
# bindkey -rpM viins '^['
# bindkey -M viins '^? ' vi-forward-char

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
#
# Also be sure to set this in tmux.conf: `set -s escape-time 0`.
#
# https://github.com/softmoth/zsh-vim-mode#keytimeout
export KEYTIMEOUT=20
# Unbind double escape
bindkey -rpM viins '^[^['

bindkey -v  # vi mode

bindkey -M viins '^A' history-incremental-search-backward
bindkey -M viins '^R' history-incremental-search-backward

source_all "${ZSH_CONFIG_DIR}/zsh.d/vi"
