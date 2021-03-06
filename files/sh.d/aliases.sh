alias jq_clipboard="xsel -ob | jq | xsel -ib"

## Git
alias ga='git add'
alias gb='git branch'
alias gbD='git branch -D'
alias gc='git checkout'
alias gcl='git clone --depth 20'
alias gcm='git commit -m'
alias gcmm='git commit -m more'
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
alias grv='git remote -v'
alias gs='git status'
alias gss='git status --short'
alias gwl='git worktree list'
alias gwa='git worktree add'
alias gwm='git worktree move'
alias gwr='git worktree remove'

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
alias kmonad-mini='z kmon && sudo echo && sudo kmonad ~/code/dots/files/kmonad/mini.kbd & disown'

alias sshb='ssh big'
alias sshbe='ssh big-external'

alias ag='ag --path-to-ignore ~/code/dots/files/agignore'
alias makej='make -j$(nproc)'
alias lock='systemctl start physlock'
alias mattermost='bash ~/code/dots/files/scripts/mattermosts.sh'

alias mate-docker-pull='docker pull artifactory.galois.com:5004/mate-dev:master && docker tag artifactory.galois.com:5004/mate-dev:master mate-dev && docker pull artifactory.galois.com:5004/mate-dist:master && docker tag artifactory.galois.com:5004/mate-dist:master mate-dist && docker image prune'
alias mate-clean-submodule-integration-framework='pushd submodules/integration_framework && sudo git clean -xdf && sudo git reset --hard HEAD && popd'

# temporary

alias restart_qute='kill -9 $(pgrep qutebrowser) && qutebrowser 2>&1 > /dev/null & disown'
alias restart_steam='kill -9 $(pgrep steam) && steam 2>&1 > /dev/null & disown'

# Haskell

alias entr-hlint='fd . --extension hs | entr -c -s "hlint --hint=$HOME/code/dots/files/hlint.yaml src"'
