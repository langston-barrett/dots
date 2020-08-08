#!/usr/bin/env bash

# shellcheck disable=SC2039

# Modified from:
# https://gist.github.com/dshnkao/10865f32d69e40dc591e08e3af970e9d

set -e

db_path=${1}
if [[ -z $db_path ]]; then
  db_path=$(ls "$HOME"/.mozilla/firefox/*.default/places.sqlite)
fi

tmp=$(mktemp)
cp "$db_path" "$tmp"

query="
SELECT
    url, title FROM moz_places
WHERE
    url NOT LIKE '%google%search%'
ORDER BY
    visit_count DESC,
    last_visit_date DESC;
"

SEP="∙"

ENTRY=$(
  sqlite3 "$tmp" "$query" | \
    sed -E 's/^https?:\/\///' | \
    sed -E "s/\\/?\\|/ $SEP /" | \
    sed -E "s/$SEP $//" | \
    rofi -dmenu --no-sort)

URL=$( echo "$ENTRY" | sed "s/$SEP.*//g" )

if [ "$URL" = "" ]; then
  exit 0
fi

# google search if input end with .
if [ "${URL: -1}" = "." ]; then
  SEARCH="${URL:: -1}"
  URL="google.com/search?q=$SEARCH"
fi

case $(uname) in
  'Linux')
    xdg-open "https://$URL"
    ;;
  'Darwin')
    open "https://$URL"
    ;;
esac
