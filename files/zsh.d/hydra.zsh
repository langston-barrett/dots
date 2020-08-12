if [[ -n "$HYDRA" ]]; then
  unset HYDRA
  # NTS: exec does not work here
  # shell_hydra ~/code/dots/files/shell-hydra.json; exit

  setopt LOCAL_OPTIONS NO_NOTIFY NO_MONITOR
  i3-msg 'no_focus [class="conky"]' &> /dev/null
  conky --config=$HOME/code/dots/files/conky/default.conkyrc &> /dev/null &
  pid=$!
  i3-msg '[title="hydra"]' border pixel 5 &> /dev/null &
  while true; do sleep 0.05 && i3-msg '[title="hydra"]' focus &> /dev/null; done &
  shell_hydra ~/code/dots/files/shell-hydra.json 2> /dev/null
  exec kill "${pid}"
fi
