# Lazy-load fasd
z() {
  eval "$(fasd --init posix-alias zsh-hook)"
  fasd_cd -d "$@"
}
