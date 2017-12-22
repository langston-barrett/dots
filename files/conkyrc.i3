conky.config = {
  out_to_x=false,
  out_to_console=true,
  max_text_width=0,
  total_run_times=0,
}

-- | ${if_up wlp1s0} YES ${else} NO ${endif}\
conky.text = [[
🕒 : ${tztime America/Chicago %H:%M} \
| \
🐏${if_match ${memperc} > 90} (⚠)${endif} \
: ${if_match ${memperc}<10} ${endif}${memperc} \
| \
💡 : ${if_match ${cpu cpu0}<100} ${endif}${if_match ${cpu cpu0}<10} ${endif}${cpu cpu0} \
| \
💿 : ${fs_used_perc /} \
| \
${if_match "${acpiacadapter}" == "on-line"}🔌${else}\
🔋${if_match ${battery_percent BAT1} < 10} (⚠)\
${endif}${endif} : ${battery_percent BAT1} \
|\
]]