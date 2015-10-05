function git_commits_behind() {
  local curr_br=$(git symbolic-ref --short HEAD)
  echo $(git rev-list --count $curr_br..origin/$curr_br)
}
