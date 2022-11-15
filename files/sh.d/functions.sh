# Delete all merged git branches. Use caution, and only use on master.
# http://goo.gl/r9Bos0
clean-merged() {
  git branch --merged | grep -v "\*" \
    | grep -v "$(git-branch-name)" \
    | xargs -n 1 git branch -d
}

cpr() { cp "${2}" "${1}"; }

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

git-branch-name() {
  git symbolic-ref --short HEAD 2> /dev/null
}

git-delete-untracked() {
  for f in $(git ls-files --others --exclude-standard); do
    if prompt-confirm "Want to delete $f?"; then
      tp "$f"
    fi
  done
}

list-desktop-files() {
  dirs="${XDG_DATA_HOME:-${HOME}/.local/share}:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
  for d in $(array ":" "${dirs}"); do
    if [[ -d "${d}/applications" ]]; then
      fd . "${d}/applications" --extension desktop | while read -r f; do
        printf '%s\n' "${f}"
      done
    fi
  done
}

list-executables-on-path() {
  for d in $(echo "${PATH}" | sed -e 's/\:/\ /g'); do
    pushd "${d}" &> /dev/null || continue
    fd . --type x --follow --max-depth 1
    popd &> /dev/null || return
  done
}

list-history() {
  fc -l 1 | seds '^ *[[:digit:]]*\**  ' ''
}

list-make-targets() {
  make -qp | \
    awk -F':' '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}' | \
    sort -u
}

list-man-pages() {
  apropos . | awk '{ print $1 }' 
}

mount-encrypted() {
  labels=$(ls --color=never /dev/disk/by-label)
  sudo cryptsetup luksOpen "${1}" encrypted
  sudo mkdir -p "/mnt/${labels}"
  sudo mount /dev/mapper/encrypted "/mnt/${labels}"
}

nix-run-from-unstable () {
  nix-shell -p "with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) { }; $1" --run "$1 & disown"
}

# https://stackoverflow.com/questions/3231804/
prompt-confirm() {
  while true; do
    read -r -n 1 -p "${1:-Continue?} [y/n]: " REPLY
    case $REPLY in
      [yY]) echo ; return 0 ;;
      [nN]) echo ; return 1 ;;
      *) printf " \033[31m %s \n\033[0m" "invalid input"
    esac
  done
}

readlog() {
  if [[ -z "$1" ]]; then
    batp --plain log
  fi
  batp --plain "$1"
}

read-json() {
  f="${1}"
  if [[ -z "${f}" ]]; then
    f=$(fd --max-depth 1 --extension json | fzf)
  fi
  bat "${f}" | jq . | bat --file-name "${1}" --paging=always --language=json
}

# Recursively find and replace
sedr() { ag -g '.*' -0 | xargs -0 sed -E -i "$@"; }

sorted-diff() {
    file1=$(mktemp)
    file2=$(mktemp)
    bat "${1}" | sort > "${file1}"
    bat "${2}" | sort > "${file2}"
    diff "${file1}" "${file2}"
}

snip() {
  pushd ~/code/dots/files/emacs/snippets || true
  language=yaml
  if [[ -z "${1}" ]]; then
    d=$(fd --type d . | fzf --height=10% --layout=reverse)
    pushd "${d}" || true
  else
    pushd "${1}"* || true
  fi
  language=$(basename "$(pwd)")
  language="${language%-mode}"
  fd --type f . | \
    fzf --ansi --layout=reverse \
      --preview="grep -v '^#' {} | bat --language=${language} --force-colorization" | \
    xargs bat | \
    grep -v '^#' | \
    xsel -ib
  popd || return
  popd || return
}

yank-file() {
  bat "${1}" |& xsel -ib
}
