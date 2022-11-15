
tab() {
  local start_col=0
  if [[ "${#BUFFER}" -lt $((COLUMNS-32)) ]]; then
    start_col="${#BUFFER}"
  fi

  zstyle ":fzf-tab:*" fzf-flags \
    --margin=0,0,0,"${start_col}" \
    --height=10% \
    --layout=reverse \
    --prompt='' \
    --info=hidden \
    --bind="bspace:backward-delete-char/eof" \
    --bind="tab:accept" \
    --bind="space:print-query"
  fzf-tab-complete 
}
zle -N tab

bindkey -M emacs "^I" tab
bindkey -M viins "^I" tab

zstyle ':fzf-tab:complete:*:*' fzf-preview 'preview.sh ${(Q)realpath}'

zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
	fzf-preview 'echo ${(P)word}'

zstyle ':fzf-tab:complete:-command-:*' fzf-preview \
  ¦ '(out=$(tldr --color always "$word") 2>/dev/null && echo $out) || (out=$(MANWIDTH=$FZF_PREVIEW_COLUMNS man "$word") 2>/dev/null && echo $out) || (out=$(which "$word") && echo $out) || echo "${(P)word}"' 
