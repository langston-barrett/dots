#!/bin/sh

xrdb -merge ~/.Xresources
feh --bg-fill ~/.config/wallpaper.jpg
systemctl --user restart xcompmgr.service

echo "Remember to set about:config.pixelsperpx"
echo "and restart X"
