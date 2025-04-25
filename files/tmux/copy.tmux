# :lang bash

set -g copy-command "xsel -ib"
bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel
bind-key -T copy-mode-vi v send-keys -X begin-selection
bind-key -T copy-mode-vi i copy-mode -q

# https://ianthehenry.com/posts/tmux-copy-last-command/
#
# u2003 is an em-space
bind-key -T copy-mode-vi L {
  copy-mode
  send -X clear-selection
  send -X start-of-line
  send -X start-of-line
  send -X cursor-up
  send -X cursor-up
  send -X cursor-up
  send -X cursor-up
  send -X start-of-line
  send -X start-of-line

  if -F "#{m:*\u2003*,#{copy_cursor_line}}" {
    send -X search-forward-text "\u2003"
    send -X stop-selection
    send -X -N 2 cursor-right
    send -X begin-selection
    send -X end-of-line
    send -X end-of-line
    if "#{m:*\u2003?*,#{copy_cursor_line}}" {
      send -X cursor-left
    }
  } {
    send -X end-of-line
    send -X end-of-line
    send -X begin-selection
    send -X search-backward-text "\u2003"
    send -X end-of-line
    send -X end-of-line
    send -X cursor-right
    send -X stop-selection
  }
}
