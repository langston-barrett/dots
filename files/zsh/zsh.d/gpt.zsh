#!/usr/bin/env bash

shell_prompt="Provide only ZSH commands for Linux without any description. If there is a lack of details, provide most logical solution. Ensure the output is a valid shell command. If multiple steps required try to combine them together on a single line."
_shell_prompt_2="You are a Command Line Interface expert and your task is to provide functioning ZSH commands. Return a CLI command and nothing else - do not send it in a code block, quotes, or anything else, just the pure text CONTAINING ONLY THE COMMAND. If possible, return a one-line ZSH command or chain many commands together. Return ONLY the command ready to run in the terminal. The command should do the following:"

function chatgpt_raw() {
  if [[ -z ${OPENAI_KEY} ]]; then
    printf "%s\n" "Please set OPENAI_KEY" 1>&2
    return
  fi
  curl https://api.openai.com/v1/chat/completions \
    --silent \
    --show-error \
    --fail \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer ${OPENAI_KEY}" \
    -d "{
          \"model\": \"gpt-3.5-turbo\",
          \"messages\": [{\"role\": \"user\", \"content\": \"${1}\"}],
          \"max_tokens\": 1024,
          \"temperature\": 0.7
        }"
}

function chatgpt() {
  prompt=${1:-$(bat /dev/stdin)}
  chatgpt_raw "${prompt}" \
    | jq -r '.choices[0].message.content' \
    | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}

function shell-help() {
  chatgpt "${shell_prompt} ${1}"
}

function accept-line() {
  pfx='/s '
  if [[ $BUFFER == ${pfx}* ]]; then
    # add to history
    print -s ${BUFFER}
    LBUFFER=$(shell-help "${BUFFER#$pfx}")
  else
    zle .accept-line
  fi
}

zle -N accept-line
