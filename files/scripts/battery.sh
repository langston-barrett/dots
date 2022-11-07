while :
do
  rem_bat=$(acpi -b | grep -Eo "[0-9]+%" | grep -Eo "[0-9]+")
  if [[ ${rem_bat} -le 20 ]]; then
    notify-send "Battery low! ${rem_bat}"
  elif [[ ${rem_bat} -le 10 ]]; then
    notify-send -u critical "Battery low! ${rem_bat}"
  fi
  sleep 30
done
