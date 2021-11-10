git_list_remote_branches() {
  for remote in $(git remote show); do
    for branch in $(git branch --all --format='%(refname:short)'); do
      if [[ "${branch}" = "${remote}"* ]]; then
        echo "${branch##${remote}/}"
      fi
    done
  done
}

git_list_checkout_targets() {
  printf \
    "%s\n%s\n%s" \
    "$(git branch --all --format='%(refname:short)')" \
    "$(git_list_remote_branches)" \
    "$(git tag -l)" | sort | uniq
}
