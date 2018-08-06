#!/bin/sh

xrandr --output HDMI2 --off --output HDMI1 --off \
       --output DP1 --primary --mode 3840x2160 --pos 0x0 --rotate normal \
       --output eDP1 --mode 1920x1080 --pos 3840x896 --rotate normal

xrandr --dpi 190

sed -i 's/\([[:space:]]\)\+:size 18/\1:size 28/'    ~/code/dots/files/spacemacs
sed -i 's/xft:Hack:size=../xft:Hack:size=12/'       ~/.Xresources
sed -i 's/Xft.dpi:        160/Xft.dpi:        192/' ~/.Xresources

bash $HOME/.config/i3/scripts/post.sh
