#!/usr/bin/env zsh

#### tmux dracula theme

# ## Colors
local black='colour16'
local white='colour255'
local gray='colour236'
local dark_gray='colour236'
local yellow='colour215'
local light_purple='colour141'
local dark_purple='colour61'

# ## Icons
local left_sep=''
local right_sep=''
local right_alt_sep=''

tmux set-option status on
tmux set-option status-left-length 100
tmux set-option status-right-length 100
tmux set-option status-bg "${dark_gray}"
tmux set-option pane-active-border-fg "${dark_purple}"
tmux set-option pane-border-fg "${gray}"
tmux set-option message-bg "${gray}"
tmux set-option message-fg "${white}"
tmux set-option message-command-bg "${gray}"
tmux set-option message-command-fg "${white}"
tmux set-option status-left " #I #[fg=${dark_gray},reverse]${right_sep} "
tmux set-option status-left-style "fg=${white},bg=${dark_purple},bold"
tmux set-option status-right "${left_sep}#[bg=${black},reverse] %Y-%m-%d %H:%M "
tmux set-option status-right-style "fg=${light_purple},bg=${dark_gray}"
tmux set-window-option window-status-activity-style "fg=${white},bg=${gray}"
tmux set-window-option window-status-separator ''
tmux set-window-option window-status-format ' #I #W '
tmux set-window-option window-status-style "fg=${yellow},bg=${dark_gray}"
tmux set-window-option window-status-current-format \
  "${right_sep}#[fg=${black}] #I ${right_alt_sep} #W #[fg=${dark_gray},reverse]${right_sep}"
tmux set-window-option window-status-current-style "fg=${dark_gray},bg=${light_purple}"
