#!/usr/bin/env zsh

SSH_ENV="$HOME/.ssh/environment"

function start_agent {
  echo "Initialising new SSH agent..."
  ssh-agent | sed 's/^echo/#echo/' > "$SSH_ENV"
  echo succeeded
  chmod 600 "$SSH_ENV"
  . "$SSH_ENV" > /dev/null
  ssh-add;
}

# Source SSH settings, if applicable

if [[ -f "$SSH_ENV" ]]; then
  . "$SSH_ENV" > /dev/null
  # TODO: we assume it's still running
  #ps $SSH_AGENT_PID doesn't work under cywgin
  # ps -ef | grep $SSH_AGENT_PID | grep ssh-agent$ > /dev/null || {
  #   start_agent;
  # }
else
  start_agent;
fi
