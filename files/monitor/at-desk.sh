#!/bin/sh

xrandr \
  --output DP1 --primary --mode 3840x2160 --pos 0x0 --rotate normal \
  --output eDP1 --off \
  --output DP2 --off \
  --output HDMI1 --off \
  --output HDMI2 --off \
  --output VIRTUAL1 --off

# xrandr --dpi 190

# sed -i 's/\([[:space:]]\)\+:size 18/\1:size 28/'    ~/code/dots/files/spacemacs
# sed -i 's/xft:Hack:size=../xft:Hack:size=12/'       ~/.Xresources
# sed -i 's/Xft.dpi:        160/Xft.dpi:        192/' ~/.Xresources
# sed -i 's/\([[:space:]]\)\+size 9.0/\1size 11.0/'    ~/code/dots/files/alacritty.yml

if pgrep qutebrowser; then
  qutebrowser ":set zoom.default 150%"
  qutebrowser ":set fonts.default_size 20px"
fi

bash "$HOME/.config/i3/scripts/post.sh"
