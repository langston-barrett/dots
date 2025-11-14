#!/usr/bin/env bash
# ^-- for shellcheck

is_bin_on_path() { builtin type -P "${1}" &> /dev/null; }
if is_bin_on_path direnv && [[ -z ${IN_NIX_SHELL} ]]; then
  eval "$(direnv hook bash)"
fi
