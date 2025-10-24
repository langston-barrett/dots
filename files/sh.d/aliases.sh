export PATH=$PATH:${HOME}/code/dots/files/scripts/bin/

export EDITOR=hx
ee() {
  if [[ -n "${1}" ]]; then
    hx "$@"
  else
    hx "$(fd . --type f --max-depth 5 | zlefzff)"
  fi
}

alias jq_clipboard="xsel -ob | jq | xsel -ib"

lhd() {
  lesshand-cli decode "${1:--}"
}

lhe() {
  f=$(mktemp)
  "${EDITOR}" "${f}"
  lesshand-cli decode "${f}" | copy
}

mkcd() {
  mkdir "${1}" && cd "${1}"
}

seds() {
  sed "$(printf 's|%s|%s|g' "${1}" "${2}")"
}

llvm-dis-view() {
  f="${1}"
  shift 1
  llvm-dis -o - "${f}" | bat --language=llvm --file-name="${f}.ll" "${@}"
}

# For copy/pasting from websites
if [[ -n "${ZSH_NAME}" ]]; then
  undollar() { "${@}"; }
  alias '$'='undollar'
  alias '#'='undollar'
fi

upstream_name() {
    git rev-parse --symbolic-full-name --abbrev-ref @{u}
}

# https://stackoverflow.com/questions/37648908/determine-if-a-merge-will-resolve-via-fast-forward
#
# canff - test whether it is possible to fast-forward to
# a given commit (which may be a branch name).  If given
# no arguments, find the upstream of the current (HEAD) branch.
canff() {
  branch_name=main
  if [ $# -gt 0 ]; then  # at least 1 argument given
      branch_name="$1"
      # make sure it is or can be converted to a commit ID.
      git rev-parse -q --verify "${branch_name}^{commit}" >/dev/null || {
          printf "%s: not a valid commit specifier\n" "${branch_name}"
          return 1
      }
  else
    # no arguments: find upstream, or bail out
    branch_name=$(upstream_name) || return $?
  fi
  # now test whether git merge --ff-only could succeed on $b
  if git merge-base --is-ancestor HEAD "${branch_name}"; then
    echo "merge with ${branch_name} can fast-forward"
  else
    echo "merge with ${branch_name} cannot fast-forward"
  fi
}

maxmem() {
  gb="${1}"
  shift
  systemd-run --scope -p MemoryMax="${gb}G" --user "${@}"
}

alias kmonad-minidox='cd /tmp && sudo echo && sudo nohup kmonad ~/code/dots/files/kmonad/minidox.kbd & disown'
alias keyb='cd /tmp && sudo echo && sudo nohup kmonad ~/code/dots/files/kmonad/minidox.kbd & disown'
alias lkeyb='cd /tmp && sudo echo && sudo nohup kmonad ~/code/dots/files/kmonad/x1.kbd & disown'
alias kmonad-minidox4="cd /tmp ; sudo echo ; sudo nohup ${HOME}/.cabal/bin/kmonad ${HOME}/code/dots/files/kmonad/minidox-0.4.kbd & disown"
alias kmonad-mini='z kmon && sudo echo && sudo kmonad ~/code/dots/files/kmonad/mini.kbd & disown'
alias makej='make -j$(nproc)'
alias lock='systemctl start physlock'

open() { xdg-open "${1}" & disown; }

# kludge expand --aliases "" ""

alias ba='cabal build all'
alias bc='clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0'
alias cb='cabal'
alias cg='cargo'
alias cgi.='cargo install --path=.'
alias cpwd='pwd | copy'
alias dk='docker'
alias e='hx'
alias ll='clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0 -S'
alias m='make'
alias od='objdump'
alias pr='gh pr create --assignee langston-barrett --web'
alias py3='python3'
alias rgall='rg --hidden --no-ignore'
alias rmrf='\rm -rf'
alias sky='ssh sky'
alias todo='hx ~/todo.md'
alias tp='trash put'
alias y='clipboard'
alias nb='nix-build'
alias nc='nix-channel'
alias nba='nix-build -A'
alias ns='nix-shell'
alias nsr='nix-shell --run'
alias ga.='git add .'
alias gc.='git commit --message .'
alias gca='git commit --amend'
alias gcb='git checkout -b'
alias gclg='git clone https://github.com/GaloisInc/'
alias gclh='git clone https://github.com/'
alias gclm='git clone https://github.com/langston-barrett/'
alias gco-='git checkout -'
alias gds='git diff --cached'
alias gfo='git fetch origin'
alias gfu='git fetch upstream'
alias gplm='git pull mine'
alias gplo='git pull origin'
alias gplu='git pull upstream'
alias grph='git rev-parse HEAD'
alias grv='git remote --verbose'
alias gsuud='git submodule update'
alias gsuudi='git submodule update'
alias k='kludge'
alias ka='hx ~/code/dots/kludge/src/expand.rs'
alias ki='cd ~/code/dots/kludge; cargo install --path=.; cd -'
