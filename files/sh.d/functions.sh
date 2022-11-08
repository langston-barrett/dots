extract() {
  f="${1}"
  if [[ -z "${f}" ]]; then
    f=$(fd --max-depth 1 --extension tar --extension gz --extension xz --extension zip | fzf)
  fi
  if [[ "${f}" == *tar ]] || \
     [[ "${f}" == *tar.bz ]] || \
     [[ "${f}" == *tar.bz2 ]] || \
     [[ "${f}" == *tar.gz ]] || \
     [[ "${f}" == *tar.xz ]]; then
    tar xvf "${f}"
  elif [[ "${f}" == *gz ]]; then
    gunzip "${f}"
  elif [[ "${f}" == *zip ]]; then
    unzip "${f}"
  fi
}

read-json() {
  f="${1}"
  if [[ -z "${f}" ]]; then
    f=$(fd --max-depth 1 --extension json | fzf)
  fi
  bat "${f}" | jq . | bat --file-name "${1}" --paging=always --language=json
}
