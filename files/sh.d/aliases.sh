alias jq_clipboard="xsel -ob | jq | xsel -ib"

## Git
alias ga='git add'
alias gb='git branch'
alias gbD='git branch -D'
alias gcl='git clone --depth 20'
alias gcm='git commit -m'
alias gcmm='git commit -m .'
alias gca='EDITOR=vi git commit --amend'
alias gcb='git checkout -b'
alias gd='git diff'
alias gds='git diff --cached'
alias gdm='git diff master'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gFp='git pull origin'
alias gFu='git pull upstream'
alias gm='git merge'
alias gmum='git merge upstream/master'
alias gp='git checkout master && git pull && git checkout -'
alias gpum='git pull upstream master'
alias gPp='git push -u origin'
alias gPf='git push --force-with-lease'
alias gr='git reset'
alias grhm='git reset --hard origin/master'
alias gri='EDITOR=vi git rebase -i'
alias grc='git rebase --continue'
alias gra='git rebase --abort'
alias grv='git remote -v'
alias gs='git status'
alias gss='git status --short'
alias gwl='git worktree list'
alias gwa='git worktree add'
alias gwm='git worktree move'
alias gwr='git worktree remove'

git_choose_branch() {
  git branch --all --format='%(refname:short)' | fzf
}

gc() {
  if [[ -n "${1}" ]]; then
    git checkout "$@"
  else
    git checkout $(git_choose_branch)
  fi
}


github_clone() { git clone "https://github.com/${1}"; }
git_clone_mine() { git clone "https://github.com/langston-barrett/${1}"; }
git_remote_add_sky() {
  git remote add sky "ssh://langston@sky/home/langston/code/${1}"
}

# https://stackoverflow.com/questions/37648908/determine-if-a-merge-will-resolve-via-fast-forward
#
# canff - test whether it is possible to fast-forward to
# a given commit (which may be a branch name).  If given
# no arguments, find the upstream of the current (HEAD) branch.

upstream_name() {
    git rev-parse --symbolic-full-name --abbrev-ref @{u}
}

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
alias kmonad-minidox4="cd /tmp ; sudo echo ; sudo nohup ${HOME}/.cabal/bin/kmonad ${HOME}/code/dots/files/kmonad/minidox-0.4.kbd & disown"
alias kmonad-mini='z kmon && sudo echo && sudo kmonad ~/code/dots/files/kmonad/mini.kbd & disown'

alias sshb='ssh big'
alias sshbe='ssh big-external'

alias ag='ag --path-to-ignore ~/code/dots/files/agignore'
alias makej='make -j$(nproc)'
alias lock='systemctl start physlock'
alias mattermost='bash ~/code/dots/files/scripts/mattermosts.sh'

alias mate-docker-pull='docker pull ghcr.io/galoisinc/mate-dev:main && docker tag ghcr.io/galoisinc/mate-dev:main mate-dev && docker pull ghcr.io/galoisinc/mate-dist:main && docker tag ghcr.io/galoisinc/mate-dist:main mate-dist && docker image prune'
alias mate-docker-oss-pull='docker pull artifactory.galois.com:5004/mate-dev:open-source && docker tag artifactory.galois.com:5004/mate-dev:open-source mate-dev && docker pull artifactory.galois.com:5004/mate-dist:open-source && docker tag artifactory.galois.com:5004/mate-dist:open-source mate-dist && docker image prune'
alias polymorph-docker-pull='docker pull artifactory.galois.com:5016/polymorph/dist:main && docker tag artifactory.galois.com:5016/polymorph/dist:main polymorph-dist && docker pull artifactory.galois.com:5016/polymorph/dist:dev && docker tag artifactory.galois.com:5016/polymorph/dist:dev polymorph-dev'
alias mate-clean-submodule-integration-framework='pushd submodules/integration_framework && sudo git clean -xdf && sudo git reset --hard HEAD && popd'
alias d='sudo -g docker zsh'

# temporary

alias restart_qute='kill -9 $(pgrep qutebrowser) && qutebrowser 2>&1 > /dev/null & disown'
alias restart_steam='kill -9 $(pgrep steam) && steam 2>&1 > /dev/null & disown'

# Haskell

alias entr-hlint='fd . --extension hs | entr -c -s "hlint --hint=$HOME/code/dots/files/hlint.yaml src"'

for f in ~/org/code/dots/sh.d/*.sh; do
  . "${f}"
done
