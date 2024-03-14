z() {
  if [[ -n "${ZSH_NAME}" ]]; then
    eval "$(zoxide init zsh)"
  else
    eval "$(zoxide init bash)"
  fi
  __zoxide_z "${@}"
}
