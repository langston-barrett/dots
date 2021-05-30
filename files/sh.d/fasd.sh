# Lazy-load fasd
z() {
  if [[ -n "${ZSH_NAME}" ]]; then
    eval "$(fasd --init posix-alias zsh-hook)"
  else
    eval "$(fasd --init posix-alias bash-hook)"
  fi
  fasd_cd -d "$@"
}
