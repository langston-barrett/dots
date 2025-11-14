#!/usr/bin/env bash
# ^-- for shellcheck

# shellcheck disable=SC2034

HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=10000

# don't append "not found command" to history
# https://www.zsh.org/mla/users//2014/msg00715.html
# zshaddhistory() { whence ${${(z)1}[1]} >| /dev/null || return 1 }

# https://zsh.sourceforge.io/Doc/Release/Options.html
setopt HIST_FIND_NO_DUPS    # Do not display a line previously found.
setopt HIST_IGNORE_ALL_DUPS # Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_DUPS     # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_SPACE    # Don't record an entry starting with a space.
setopt HIST_REDUCE_BLANKS   # Remove superfluous blanks before recording entry.
setopt HIST_SAVE_NO_DUPS    # Don't write duplicate entries in the history file.
setopt HIST_VERIFY          # Show command with history expansion to user before running it.
setopt INC_APPEND_HISTORY   # Don't wait until shell exits to write to history.
setopt SHARE_HISTORY        # Share history in every terminal session.
