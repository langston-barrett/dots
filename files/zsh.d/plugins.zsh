plugin_dir=~/.config/zsh/plugins
for f in ~/.zsh.d/plugins/*.zsh; do
  source "${f}"
done

# TODO: https://github.com/marlonrichert/zsh-autocomplete