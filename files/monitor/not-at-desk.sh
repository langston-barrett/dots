#!/bin/sh

xrandr --output HDMI2 --off --output HDMI1 --off --output DP1 --off \
       --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal

xrandr --dpi 160

sed -i 's/\([[:space:]]\)\+:size 28/\1:size 18/'    ~/code/dots/files/spacemacs
sed -i 's/xft:Hack:size=28/xft:Hack:size=18/'       ~/.Xresources
sed -i 's/Xft.dpi:        192/Xft.dpi:        160/' ~/.Xresources
sed -i 's/\([[:space:]]\)\+size 11.0/\1size 9.0/'    ~/code/dots/files/alacritty.yml

bash $HOME/.config/i3/scripts/post.sh
