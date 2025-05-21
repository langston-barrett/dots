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

if [[ ${OSTYPE} == darwin* ]]; then
  clipboard() {
    if [[ -n "${1}" ]]; then
      pbcopy <"${1}"
    else
      pbcopy
    fi
  }
else
  clipboard() {
    if [[ -n "${1}" ]]; then
      xsel -ib <"${1}"
    else
      xsel -ib
    fi
  }
fi

seds() {
  sed "$(printf 's|%s|%s|g' "${1}" "${2}")"
}

llvm-dis-view() {
  f="${1}"
  shift 1
  llvm-dis -o - "${f}" | bat --language=llvm --file-name="${f}.ll" "${@}"
}

alias trailing="sed -i 's/[ \t]*$//'"

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

alias bc='clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0'
alias cb='cabal'
alias cg='cargo'
alias dk='docker'
alias e='hx'
alias ll='clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -O0 -S'
alias m='make'
alias od='objdump'
alias py3='python3'
alias rgall='rg --hidden --no-ignore'
alias rmrf='\rm -rf'
alias sky='ssh sky'
alias tp='trash put'
alias y='clipboard'
alias nb='nix-build'
alias nba='nix-build -A'
alias ns='nix-shell'
alias nsr='nix-shell --run'
alias nsrzsh='nix-shell --run 'exec zsh''
alias gclh='git clone https://github.com/'
alias gclg='git clone https://github.com/GaloisInc/'
alias gclm='git clone https://github.com/langston-barrett/'
alias ga='git add'
alias gau='git add --update'
alias gb='git branch'
alias gbD='git branch -D'
alias gbl='git blame'
alias gbr='git branch'
alias gca='git commit --amend'
alias gcb='git checkout -b'
alias gcl='git clone --jobs 4'
alias gcm='git commit -m'
alias gcm='git commit'
alias gcmm='git commit --message .'
alias gco='git checkout'
alias gcom='git checkout main'
alias gcp='git cherry-pick'
alias gd='git diff'
alias gdm='git diff master'
alias gds='git diff --cached'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gFp='git pull origin'
alias gFu='git pull upstream'
alias ghd='git rev-parse HEAD'
alias gi='git init'
alias gl='git log'
alias glsf='git ls-files'
alias gm='git merge'
alias gmum='git merge upstream/master'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gPf='git push --force-with-lease'
alias gpl='git pull'
alias gplm='git pull mine'
alias gplo='git pull origin'
alias gplu='git pull upstream'
alias gPp='git push -u origin'
alias gpum='git pull upstream master'
alias gr='git reset'
alias gra='git rebase --abort'
alias grb='git rebase'
alias grc='git rebase --continue'
alias grhm='git reset --hard origin/master'
alias gri='git rebase --interactive'
alias grv='git remote --verbose'
alias gs='git status'
alias gsh='git stash'
alias gss='git status --short'
alias gsu='git submodule'
alias gsup='git submodule update'
alias gsupi='git submodule update --init'
alias gt='git tag'
alias gwa='git worktree add'
alias gwl='git worktree list'
alias gwm='git worktree move'
alias gwr='git worktree remove'
