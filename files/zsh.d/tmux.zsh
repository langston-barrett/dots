#!/usr/bin/env zsh

#### tmux dracula theme

## Colors
local black='colour16'
local white='colour255'
local gray='colour236'
local dark_gray='colour236'
local yellow='colour215'
local light_purple='colour141'
local dark_purple='colour61'

## Icons
local left_sep=''
local right_sep=''
local right_alt_sep=''

tmux set-option -g status on
tmux set-option -g status-left-length 100
tmux set-option -g status-right-length 100
tmux set-option -g status-bg "${dark_gray}"
tmux set-option -g pane-active-border-fg "${dark_purple}"
tmux set-option -g pane-border-fg "${gray}"
tmux set-option -g message-bg "${gray}"
tmux set-option -g message-fg "${white}"
tmux set-option -g message-command-bg "${gray}"
tmux set-option -g message-command-fg "${white}"
tmux set-option -g status-left " #I #[fg=${dark_gray},reverse]${right_sep} "
tmux set-option -g status-left-style "fg=${white},bg=${dark_purple},bold"
tmux set-option -g status-right "${left_sep}#[bg=${black},reverse] %Y-%m-%d %H:%M "
tmux set-option -g status-right-style "fg=${light_purple},bg=${dark_gray}"
tmux set-window-option -g window-status-activity-style "fg=${white},bg=${gray}"
tmux set-window-option -g window-status-separator ''
tmux set-window-option -g window-status-format ' #I #W '
tmux set-window-option -g window-status-style "fg=${yellow},bg=${dark_gray}"
tmux set-window-option -g window-status-current-format \
  "${right_sep}#[fg=${black}] #I ${right_alt_sep} #W #[fg=${dark_gray},reverse]${right_sep}"
tmux set-window-option -g window-status-current-style "fg=${dark_gray},bg=${light_purple}"
