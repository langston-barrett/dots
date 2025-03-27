# Lazy-load fasd
z() {
  unset -f z
  if [[ -n "${ZSH_NAME}" ]]; then
    eval "$(zoxide init zsh)"
  else
    eval "$(zoxide init bash)"
  fi
  __zoxide_z "${@}"
}
zz() { ssh -t "${1}" "eval $(fasd --init posix-alias bash-hook); fasd_cd -d ${2}; exec \${SHELL} -l"; }
