# Lazy-load fasd
z() {
  if [[ -n "${ZSH_NAME}" ]]; then
    eval "$(fasd --init posix-alias zsh-hook)"
  else
    eval "$(fasd --init posix-alias bash-hook)"
  fi
  fasd_cd -d "$@"
}
zz() { ssh -t "${1}" "eval $(fasd --init posix-alias bash-hook); fasd_cd -d ${2}; exec \${SHELL} -l"; }
