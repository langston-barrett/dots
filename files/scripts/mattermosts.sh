#!/usr/bin/env bash

set -euo pipefail

host=big-wifi

if ssh "${host}" tmux list-sessions; then
  sessions=$(ssh "${host}" tmux list-sessions 2>&1)
else
  sessions=""
fi

for team in galois arcos chess; do
  session="${team}-matterhorn"
  if ! ssh "${host}" tmux has-session -t "${session}"; then
    ssh -t "${host}" tmux new-session -s "${session}" -n "${session}" -d matterhorn
  fi
  urxvt --hold -e ssh -X -t "${host}" tmux attach-session -t "${session}" & disown
done
