if [[ -z $IN_NIX_SHELL ]] && [[ -z $HYDRA ]]; then
  eval "$(direnv hook bash)"
fi
