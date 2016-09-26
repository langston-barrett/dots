#!/bin/env bash
set -e

# These variables can be set in the environment
[[ -z $warn_percent ]] && warn_percent=20
[[ -z $critical_percent ]] && critical_percent=10

# Find the battery device
battery_path=$(upower --enumerate | egrep --color=never BAT)

# Get the battery percent
percent=$(upower --show-info "$battery_path" \
    | egrep -m 1 "percentage" \
    | egrep -m 1 -o "[0-9]{1,3}")

# Decide on the urgency
urgency=normal
[[ $percent -lt $critical_percent ]] && urgency=critical

# Notify the user
if [[ $percent -lt $warn_percent ]]; then
  notify-send --urgency="$urgency" \
              --hint=int:percent:"$percent" \
              "Low battery: $percent%"
fi
