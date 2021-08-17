#!/bin/sh

xrdb -merge ~/.Xresources
feh --bg-fill ~/.config/wallpaper.jpg
systemctl --user restart xcompmgr.service
