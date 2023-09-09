# alias t='todo.sh'
# alias ta='todo.sh add'
# alias tl='todo.sh list'

export EDITOR=hx
ee() {
  if [[ -n "${1}" ]]; then
    hx "$@"
  else
    hx "$(fd . --type f --max-depth 5 | zlefzff)"
  fi
}

alias jq_clipboard="xsel -ob | jq | xsel -ib"

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

build_alias() {
  alias "${1}"="${2}"

  # Works as a gentle reminder with ZSH alias expansion on space
  if [[ -n "${ZSH_NAME}" ]]; then
    alias "${2}"="${1}"
  fi
}

## Tools

build_alias cb cabal
build_alias cg cargo
build_alias m make
# build_alias od objdump
build_alias dk docker
build_alias py python
build_alias py3 python3
build_alias tr trash
alias tp='trash put'

## Git

# See also gitconfig aliases

build_alias g git

# TODO: unused
_GIT_ALIASES=(
  "git,a,add"
  "git,b,branch"
  "git,bl,blame"
  "git,cb,checkout -b"
  "git,cl,clone --jobs 4"
  "git,co,checkout"
  "git,com,checkout main"
  "git,cp,cherry-pick"
  "git,cm,commit"
  "git,cmm,commit --message "
  "git,d,diff"
  "git,ds,diff --cached"
  "git,f,fetch"
  "git,fa,fetch --all"
  "git,i,init"
  "git,hd,rev-parse HEAD"
  "git,l,log"
  "git,lsf,ls-files"
  "git,m,merge"
  "git,p,push"
  "git,pf,push --force-with-lease"
  "git,pl,pull"
  "git,plm,pull mine"
  "git,plo,pull origin"
  "git,plu,pull upstream"
  "git,r,reset"
  "git,rh,reset --hard"
  "git,rb,rebase"
  "git,rbi,rebase --interactive"
  "git,rc,rebase --continue"
  "git,ra,rebase --abort"
  "git,rv,remote --verbose"
  "git,s,status"
  "git,sh,stash"
  "git,ss,status --short"
  "git,su,submodule"
  "git,sup,submodule update"
  "git,supi,submodule update --init"
  "git,t,tag"
)

alias ga='git add'
alias gb='git branch'
alias gbl='git blame'
alias gbD='git branch -D'
alias gbr='git branch'
alias gco='git checkout'
alias gcom='git checkout main'
alias gcp='git cherry-pick'
alias gcb='git checkout -b'
alias gcm='git commit -m'
alias gcl='git clone --jobs 4'
alias gcm='git commit'
alias gcmm='git commit --message .'
alias gca='git commit --amend'
alias gd='git diff'
alias gds='git diff --cached'
alias gdm='git diff master'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gFp='git pull origin'
alias gFu='git pull upstream'
alias gi='git init'
alias ghd='git rev-parse HEAD'
alias gl='git log'
alias glsf='git ls-files'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpl='git pull'
alias gplm='git pull mine'
alias gplo='git pull origin'
alias gplu='git pull upstream'
alias gm='git merge'
alias gmum='git merge upstream/master'
alias gpum='git pull upstream master'
alias gPp='git push -u origin'
alias gPf='git push --force-with-lease'
alias gr='git reset'
alias grhm='git reset --hard origin/master'
alias grb='git rebase'
alias gri='git rebase --interactive'
alias grc='git rebase --continue'
alias gra='git rebase --abort'
alias grv='git remote --verbose'
alias gs='git status'
alias gsh='git stash'
alias gss='git status --short'
alias gsu='git submodule'
alias gsup='git submodule update'
alias gsupi='git submodule update --init'
alias gt='git tag'
alias gwl='git worktree list'
alias gwa='git worktree add'
alias gwm='git worktree move'
alias gwr='git worktree remove'

github_clone() { git clone "https://github.com/${1}"; }
git_clone_mine() { git clone "https://github.com/langston-barrett/${1}"; }


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


# systemd
alias sys="sudo systemctl";
alias syss="sudo systemctl status";
alias sysr="sudo systemctl restart";
alias sysu="systemctl --user";
alias sysus="systemctl --user status";
alias sysur="systemctl --user restart";

maxmem() {
  gb="${1}"
  shift
  systemd-run --scope -p MemoryMax="${gb}G" --user "${@}"
}

## Nix
alias nb='nix-build'
alias nba='nix-build -A'
alias ns='nix-shell'
alias nsr='nix-shell --run'
alias nsp='nix-shell --pure'
alias nspr='nix-shell --pure --run'

# TODO: replace with "nix run || nix log"
alias nsrzsh='nix-shell --run "exec zsh"'
alias nz='nsrzsh || nsrzsh nix/shell.nix || nsrzsh tools/shell.nix'

alias kmonad-minidox='cd /tmp && sudo echo && sudo nohup kmonad ~/code/dots/files/kmonad/minidox.kbd & disown'
alias keyb='cd /tmp && sudo echo && sudo nohup kmonad ~/code/dots/files/kmonad/minidox.kbd & disown'
alias kmonad-minidox4="cd /tmp ; sudo echo ; sudo nohup ${HOME}/.cabal/bin/kmonad ${HOME}/code/dots/files/kmonad/minidox-0.4.kbd & disown"
alias kmonad-mini='z kmon && sudo echo && sudo kmonad ~/code/dots/files/kmonad/mini.kbd & disown'

alias sshb='ssh big'
alias sshbe='ssh big-external'

alias makej='make -j$(nproc)'
alias lock='systemctl start physlock'

open() { xdg-open "${1}" & disown; }

# temporary

alias restart_steam='kill -9 $(pgrep steam) && steam 2>&1 > /dev/null & disown'

# Haskell

alias entr-hlint='fd . --extension hs | entr -c -s "hlint --hint=$HOME/code/dots/files/hlint.yaml src"'
