function kludge-space() {
  if [[ "${BUFFER% }" == "${BUFFER}" ]] || [[ "${CURSOR}" != "${#BUFFER}" ]]; then
    out=$(kludge zle expand "${LBUFFER}" "${RBUFFER}")
    if [ "${?}" -eq 0 ]; then
      BUFFER=${out}
      CURSOR=${#BUFFER}
    else
      zle self-insert
    fi
  else
    # If the user entered space twice, show applicable abbrevs
    zle -M "$(kludge zle hint "${BUFFER% }")"
  fi
}

# space
zle -N kludge-space
bindkey -M emacs " " kludge-space
bindkey -M viins " " kludge-space

# control-space is a normal space
bindkey -M emacs "^ " magic-space
bindkey -M viins "^ " magic-space
