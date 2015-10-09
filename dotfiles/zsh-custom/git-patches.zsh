function git_commits_behind() {
  if git rev-parse --git-dir > /dev/null 2>&1; then
    echo $(git rev-list --count $(current_branch)..$(git rev-parse --abbrev-ref --symbolic-full-name @{u}))
  fi
}
