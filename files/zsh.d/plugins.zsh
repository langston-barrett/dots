plugin_dir=~/.config/zsh/plugins
for f in ~/.zsh.d/plugins/*.zsh; do
  source "${f}"
done

source ~/code/zsh-contextual-abbrevs/src/contextual-abbrevs.zsh

# TODO: https://github.com/marlonrichert/zsh-autocomplete