#!/bin/sh


xrandr \
  --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
  --output DP-1 --off \
  --output DP-2 --off \
  --output HDMI-1 --off \
  --output HDMI-2 --off \
  --output VIRTUAL1 --off

# xrandr --dpi 160

# sed -i 's/\([[:space:]]\)\+:size 28/\1:size 18/'    ~/code/dots/files/spacemacs
# sed -i 's/xft:Hack:size=28/xft:Hack:size=18/'       ~/.Xresources
# sed -i 's/Xft.dpi:        192/Xft.dpi:        160/' ~/.Xresources
# sed -i 's/\([[:space:]]\)\+size 11.0/\1size 9.0/'    ~/code/dots/files/alacritty.yml

if pgrep qutebrowser; then
  qutebrowser ":set zoom.default 125%"
  qutebrowser ":set fonts.default_size 16px"
fi

bash "$HOME/.config/i3/scripts/post.sh"
