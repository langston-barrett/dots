# https://github.com/mattmc3/zsh_unplugged
plugin-load() {
  local repo plugdir initfile
  ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/plugins}
  for repo in $@; do
    plugdir=$ZPLUGINDIR/${repo:t}
    initfile=$plugdir/${repo:t}.plugin.zsh
    if [[ ! -d $plugdir ]]; then
      echo "Cloning $repo..."
      git clone -q --depth 1 --recursive --shallow-submodules https://github.com/$repo $plugdir
    fi
    if [[ ! -e $initfile ]]; then
      local -a initfiles=($plugdir/*.plugin.{z,}sh(N) $plugdir/*.{z,}sh{-theme,}(N))
      (( $#initfiles )) || { echo >&2 "No init file found '$repo'." && continue }
      ln -sf "${initfiles[1]}" "$initfile"
    fi
    fpath+=$plugdir
    (( $+functions[zsh-defer] )) && zsh-defer . $initfile || . $initfile
  done
}

plugin-compile() {
  ZPLUGINDIR=${ZPLUGINDIR:-$HOME/.config/zsh/plugins}
  autoload -U zrecompile
  local f
  for f in $ZPLUGINDIR/**/*.zsh{,-theme}(N); do
    zrecompile -pq "$f"
  done
}

# fzf-tab does not like being deferred
plugin-load Aloxaf/fzf-tab
plugin-load romkatv/zsh-defer

for f in ${XDG_CONFIG_HOME:-${HOME}/.config}/zsh/zsh.d/plugins/*.zsh; do
  zsh-defer source "${f}"
done

if [[ -d ~/code/spacezle ]]; then
  zsh-defer source ~/code/spacezle/src/spacezle.zsh
fi
# if [[ -d ~/code/zsh-contextual-abbrevs ]]; then
#   zsh-defer source ~/code/zsh-contextual-abbrevs/src/contextual-abbrevs.zsh
# fi

# TODO: https://github.com/marlonrichert/zsh-autocomplete
