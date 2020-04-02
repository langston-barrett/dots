#!/usr/bin/env bash

get_progress_string() {
    # getProgressString <TOTAL ITEMS> <FILLED LOOK> <NOT FILLED LOOK> <STATUS>
    # For instance:
    # $ getProgressString 10 "#" "-" 50
    # #####-----
    # Note: if you want to use | in your progress bar string you need to change the delimiter in the sed commands

    local ITEMS="$1" # The total number of items(the width of the bar)
    local FILLED_ITEM="$2" # The look of a filled item 
    local NOT_FILLED_ITEM="$3" # The look of a not filled item
    local STATUS="$4" # The current progress status in percent

    # calculate how many items need to be filled and not filled
    FILLED_ITEMS=$(echo "((${ITEMS} * ${STATUS})/100 + 0.5) / 1" | bc)
    NOT_FILLED_ITEMS=$(echo "$ITEMS - $FILLED_ITEMS" | bc)

    # Assemble the bar string
    msg=$(printf "%${FILLED_ITEMS}s" | sed "s| |${FILLED_ITEM}|g")
    msg=${msg}$(printf "%${NOT_FILLED_ITEMS}s" | sed "s| |${NOT_FILLED_ITEM}|g")
    echo "$msg"
}

amixer sset Master "$@"
volume="$(amixer -c 0 get Master | tail -1 | awk '{print $4}' | sed 's/[^0-9]*//g')"
# mute="$(amixer -c 0 get Master | tail -1 | awk '{print $6}' | sed 's/[^a-z]*//g')"

notify-send \
  --hint=string:x-dunst-stack-tag:volume \
  "volume: $(get_progress_string 20 "#" "-" "${volume}")"
