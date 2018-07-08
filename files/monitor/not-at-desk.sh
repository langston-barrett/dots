#!/bin/sh

xrandr --output HDMI2 --off --output HDMI1 --off --output DP1 --off \
       --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal

xrandr --dpi 160

sed -i 's/\:size 28/\:size 18/'                     ~/.spacemacs
sed -i 's/xft:Hack:size=28/xft:Hack:size=18/'       ~/.Xresources
sed -i 's/Xft.dpi:        192/Xft.dpi:        160/' ~/.Xresources

xrdb -merge ~/.Xresources
feh --bg-fill ~/.config/wallpaper.jpg
