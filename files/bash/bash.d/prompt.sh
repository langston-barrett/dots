#!/usr/bin/env bash

newline=$'\n'
user="$(whoami)"
if [[ "${user}" == langston ]]; then
  user=""
else
  user="${user}@$(hostname) : "
fi
export PS1="${newline}[${user}${PROMPT_EXTRA}\w] ${newline}> "
unset newline
