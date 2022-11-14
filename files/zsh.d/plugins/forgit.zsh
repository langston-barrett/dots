mkdir -p "${plugin_dir}"

plugin=forgit

if ! [[ -d "${plugin_dir}/${plugin}" ]]; then
  git clone \
    --single-branch \
    --branch=master \
    --depth=1 \
    "https://github.com/wfxr/${plugin}" \
    "${plugin_dir}/${plugin}"
fi

export FORGIT_NO_ALIASES=1
export FORGIT_FZF_DEFAULT_OPTS="
--info=hidden
--reverse
--height '40%'
"
source "${plugin_dir}/${plugin}/${plugin}.plugin.zsh"
