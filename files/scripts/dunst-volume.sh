#!/usr/bin/env bash

amixer sset Master "$@"
volume="$(amixer -c 0 get Master | tail -1 | awk '{print $4}' | sed 's/[^0-9]*//g')"
# mute="$(amixer -c 0 get Master | tail -1 | awk '{print $6}' | sed 's/[^a-z]*//g')"
notify-send --hint=string:x-dunst-stack-tag:volume "volume: $volume"
