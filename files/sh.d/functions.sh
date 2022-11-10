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

list-executables-on-path() {
  for d in $(echo "${PATH}" | sed -e 's/\:/\ /g'); do
    pushd "${d}" &> /dev/null || continue
    fd . --type x --follow --max-depth 1
    popd &> /dev/null || return
  done
}

list-make-targets() {
  make -qp | \
    awk -F':' '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}' | \
    sort -u
}

list-man-pages() {
  apropos . | awk '{ print $1 }' 
}

read-json() {
  f="${1}"
  if [[ -z "${f}" ]]; then
    f=$(fd --max-depth 1 --extension json | fzf)
  fi
  bat "${f}" | jq . | bat --file-name "${1}" --paging=always --language=json
}

snip() {
  cd ~/code/dots/files/emacs/snippets || true
  language=yaml
  if [[ -z "${1}" ]]; then
    d=$(fd --type d . | fzf --height=10% --layout=reverse)
    cd "${d}" || true
  else
    cd "${1}"* || true
  fi
  language=$(basename "$(pwd)")
  language="${language%-mode}"
  fd --type f . | \
    fzf --ansi --layout=reverse \
      --preview="grep -v '^#' {} | bat --language=${language} --force-colorization" | \
    xargs bat | \
    grep -v '^#' | \
    xsel -ib
}
