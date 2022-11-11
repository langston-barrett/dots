zle_choose() {
  # zle_append_to_buffer "$(printf "${1}" | fzf --height=10% --layout=reverse --prompt='>> ' --bind=space:print-query)"
  zle_append_to_buffer "$(printf "${1}" | fzf --height=10% --layout=reverse --prompt='' --info=hidden)"
}

zle_append_to_buffer_newline() {
  newline=$'\n'
  BUFFER="${BUFFER%${newline}}"
  BUFFER+="${1#${newline}}"
  BUFFER="${BUFFER% }"
  CURSOR=$#BUFFER
  zle redisplay
}

zle_choose_space() {
  zle_append_to_buffer \
    "$(printf "${1}" | fzf --expect=' ' --height=10% --layout=reverse --prompt='' --info=hidden | tail -n 1)"
}

normal_space() {
  BUFFER="${BUFFER:0:${CURSOR}} ${BUFFER:${CURSOR}}"
  CURSOR="$((CURSOR+1))"
  zle redisplay
}

git_urls() {
  bat <<'EOF'
https://github.com/
https://github.com/GaloisInc
https://github.com/langston-barrett
https://gitlab-ext.galois.com/
https://gitlab-int.galois.com/
https://github.com/NixOS/nixpkgs
EOF
}

git_cmds() {
  bat <<'EOF'
add
bisect
branch
checkout
clone
commit
diff
fetch
grep
init
log
merge
mv
pull
push
pushf
rebase
remote
reset
restore
rev-parse
rm
show
stash
status
submodule
switch
tag
EOF
}

git_stash_cmds() {
  bat <<'EOF'
apply
branch
clear
create
drop
list
pop
push
remote
show
store
EOF
}

cabal_cmds() {
  bat <<'EOF'
build
check
configure
freeze
get
haddock
info
list
repl
run
test
EOF
}

cargo_cmds() {
  bat <<'EOF'
add
b
bench
bloat
c
check
clean
clippy
config
d
deb
fetch
fix
fmt
generate-lockfile
git-checkout
help
init
install
locate-project
login
logout
metadata
miri
owner
package
pkgid
publish
r
read-manifest
report
run
rustc
rustdoc
search
t
test
tree
uninstall
update
vendor
verify-project
version
yank
EOF
}

docker_cmds() {
  bat <<'EOF'
builder
buildx*
compose*
config
container
context
image
manifest
network
node
plugin
secret
service
stack
swarm
system
trust
volume
attach
build
commit
cp
create
diff
events
exec
export
history
images
import
info
inspect
kill
load
login
logout
logs
pause
port
ps
pull
push
rename
restart
rm
rmi
run
save
search
start
stats
stop
tag
top
unpause
update
version
wait
EOF
}

# Use fzf for commands with a fixed set of options, pick on enter or space
space() {
  if [[ ${AUTOFZF} != 1 ]]; then
    return
  fi

  # TODO: Only consider words that are before the cursor
  IFS=' ' read -A words <<< "${BUFFER}"
  normal_space

  num_words="${#words[@]}"
  if [[ "${words[1]}" == git ]]; then
    if [[ "${num_words}" == 1 ]]; then
      # BUFFER=""
      # zle redisplay
      zle_choose_space "$(git_cmds)"
    fi
  fi

  if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == stash ]] && [[ "${num_words}" == 2 ]]; then
    zle_choose_space "$(git_stash_cmds)"
  fi

  if [[ "${words[1]}" == cabal ]]; then
    if [[ "${num_words}" == 1 ]]; then
      zle_choose_space "$(cabal_cmds)"
    fi
  fi

  if [[ "${words[1]}" == cargo ]]; then
    if [[ "${num_words}" == 1 ]]; then
      zle_choose_space "$(cargo_cmds)"
    fi
  fi

  if [[ "${words[1]}" == docker ]]; then
    if [[ "${num_words}" == 1 ]]; then
      zle_choose_space "$(docker_cmds)"
    fi
  fi
}
zle -N space

space-space() {
  IFS=' ' read -A words <<< "${BUFFER}"
  # TODO: Only consider words that are before the cursor

  if [[ ${AUTOFZF} != 1 ]]; then
    normal_space
    return
  fi

  if [[ "${BUFFER}" == "" ]]; then
    zle_choose "$(list-executables-on-path)"
    return
  fi

  normal_space

  num_words="${#words[@]}"
  if [[ "${words[1]}" == git ]]; then
    if [[ "${num_words}" == 1 ]]; then
      zle_choose "$(git_cmds)"
    fi
  fi

  # git add
  if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == add ]] && [[ "${num_words}" == 2 ]]; then
    zle_choose "$(git_list_add_targets)"
  elif [[ "${words[1]}" == ga ]] && [[ "${#words[@]}" == 1 ]]; then
    zle_choose "$(git_list_add_targets)"
  fi

  # git checkout
  if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == checkout ]] && [[ "${num_words}" == 2 ]]; then
    zle_choose "$(git_list_checkout_targets)"
  elif [[ "${words[1]}" == gc ]] && [[ "${#words[@]}" == 1 ]]; then
    zle_choose "$(git_list_checkout_targets)"
  fi

  # git rebase
  if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == rebase ]] && [[ "${num_words}" == 2 ]]; then
    zle_choose "$(git_list_checkout_targets)"
  elif [[ "${words[1]}" == grb ]] && [[ "${#words[@]}" == 1 ]]; then
    zle_choose "$(git_list_checkout_targets)"
  fi

  # git pull
  if [[ "${words[1]}" == git ]] && [[ "${#words[@]}" -gt 1 ]]; then
    if [[ "${words[2]}" == pull ]] || [[ "${words[2]}" == pl ]]; then
      if [[ "${#words[@]}" == 2 ]]; then
        zle_choose "$(git remote show)"
      elif [[ "${#words[@]}" == 3 ]]; then
        zle_choose "$(git_list_checkout_targets)"
      fi
    fi
  fi

  if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == stash ]] && [[ "${num_words}" == 2 ]]; then
    zle_choose_space "$(git_stash_cmds)"
  fi

  if [[ "${words[1]}" == git ]] && [[ "${words[2]}" == clone ]] && [[ "${num_words}" == 2 ]]; then
    zle_choose "$(git_urls)"
  fi

  if [[ "${words[1]}" == cabal ]]; then
    if [[ "${num_words}" == 1 ]]; then
      zle_choose_space "$(cabal_cmds)"
    fi
  fi

  if [[ "${words[1]}" == cargo ]]; then
    if [[ "${num_words}" == 1 ]]; then
      zle_choose_space "$(cargo_cmds)"
    fi
  fi

  if [[ "${words[1]}" == cd ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose "$(fd . --type d --max-depth 4)"
  fi

  if [[ "${words[1]}" == cp ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose "$(fd . --max-depth 5)"
  fi

  if [[ "${words[1]}" == ee ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose "$(fd . --type f --max-depth 5)"
  fi

  if [[ "${words[1]}" == make ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose "$(list-make-targets)"
  fi

  if [[ "${words[1]}" == man ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose_space "$(list-man-pages)"
  fi

  if [[ "${words[1]}" == mv ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose "$(fd . --max-depth 5)"
  fi

  if [[ "${words[1]}" == tp ]] && [[ "${num_words}" == 1 ]]; then
    zle_choose "$(fd . --type f --max-depth 5)"
  fi
}
zle -N space-space
export AUTOFZF=1

bindkey -M emacs " " space
bindkey -M viins " " space
bindkey -M emacs "^H" space-space
bindkey -M viins "^H" space-space

# normal space during searches
bindkey -M isearch " " magic-space
