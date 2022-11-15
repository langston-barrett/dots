git_list_remote_branches() {
  for remote in $(git remote show); do
    for branch in $(git branch --all --format='%(refname:short)'); do
      if [[ "${branch}" = "${remote}"* ]]; then
        echo "${branch##${remote}/}"
      fi
    done
  done
}
