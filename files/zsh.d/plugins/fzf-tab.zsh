mkdir -p "${plugin_dir}"

if ! [[ -d "${plugin_dir}/fzf-tab" ]]; then
  git clone \
    --single-branch \
    --branch=master \
    --depth=1 \
    https://github.com/Aloxaf/fzf-tab \
    "${plugin_dir}/fzf-tab"
fi

source "${plugin_dir}/fzf-tab/fzf-tab.plugin.zsh"
