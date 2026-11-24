

PROMPT_EXIT_CODE_ERROR='✘'
PROMPT_EXIT_CODE_OK='✔'
_exit_status() {
  echo " %(?:%{$fg_bold[green]%}$PROMPT_EXIT_CODE_OK:%{$fg_bold[red]%}$PROMPT_EXIT_CODE_ERROR)"
}

SINGLE_SMALL_RIGHT_ARROW='❯'
SYMBOL_LAMBDA="λ"
RIGHT_SEPARATOR=""

GIT_SYMBOL=""
ZSH_THEME_GIT_PROMPT_UNTRACKED="?" # '✭'
ZSH_THEME_GIT_PROMPT_ADDED='✚'
ZSH_THEME_GIT_PROMPT_MODIFIED='✹'
ZSH_THEME_GIT_PROMPT_RENAMED='↺' # '➜ '
ZSH_THEME_GIT_PROMPT_DELETED='✘' # '✖'

ZSH_THEME_GIT_PROMPT_DIRTY='✘' # '●'
ZSH_THEME_GIT_PROMPT_CLEAN='✔' # '⚑'
ZSH_THEME_GIT_COMMITS_BEHIND_SUFFIX='↓' # '⇣' '⬇'

# Gives a little breathing room between status and commit diffs.
ZSH_THEME_GIT_COMMITS_AHEAD_PREFIX=' '
ZSH_THEME_GIT_COMMITS_DIVERGED='⇕'
ZSH_THEME_GIT_COMMITS_AHEAD_SUFFIX='↑' # '⇡' '⬆'


# Git segment: branch, changed/untracked state, ahead/behind
function _git_info() {
  local out
  if ! out=$(git status --porcelain=v2 --branch --ignore-submodules=dirty 2> /dev/null); then
    echo "%{%K{blue}%}"   # not in a git repo
    return
  fi

  local line oid head ahead=0 behind=0 changed=0 untracked=0
  local -a ab
  for line in "${(@f)out}"; do
    case $line in
      '# branch.oid '*)  oid=${line#\# branch.oid } ;;
      '# branch.head '*) head=${line#\# branch.head } ;;
      '# branch.ab '*)
        ab=(${=${line#\# branch.ab }})
        ahead=${ab[1]#+}
        behind=${ab[2]#-}
        ;;
      '#'*|'! '*) ;;            # other headers, ignored files
      '? '*) untracked=1 ;;
      ?*) changed=1 ;;          # modified, staged, renamed, unmerged
    esac
  done

  local branch
  if [[ $head == '(detached)' ]]; then
    branch="➦ ${oid[1,7]}"
  else
    branch="$GIT_SYMBOL $head"
  fi
  # Truncate long branch names to 40 chars; '(r.40.)' keeps the start of the name.
  if (( ${#branch} > 40 )); then
    branch="${(r.40.)branch}"
  fi

  local dirty_mark=$ZSH_THEME_GIT_PROMPT_CLEAN
  (( changed || untracked )) && dirty_mark=$ZSH_THEME_GIT_PROMPT_DIRTY

  local commits=""
  (( ahead > 0 ))  && commits+="$ZSH_THEME_GIT_COMMITS_AHEAD_PREFIX$ahead$ZSH_THEME_GIT_COMMITS_AHEAD_SUFFIX"
  (( behind > 0 )) && commits+="$ZSH_THEME_GIT_COMMITS_BEHIND_PREFIX$behind$ZSH_THEME_GIT_COMMITS_BEHIND_SUFFIX"

  local BG_COLOR=green FG_COLOR=black
  if (( untracked )); then
    BG_COLOR=red FG_COLOR=white
  elif (( changed )); then
    BG_COLOR=yellow FG_COLOR=black
  fi

  echo "%{%K{$BG_COLOR}%}%{%F{$FG_COLOR}%} $branch $dirty_mark$commits %{%F{$BG_COLOR}%K{blue}%}"
}

PROMPT_HOST='%{%b%f%}%{%F{blue}%} %n%{%F{magenta}%}@%{%F{cyan}%}%m %{%F{black}%}'
PROMPT_DIR='%{%F{white}%} %~%  '
PROMPT_SU='%(!.%{%k%F{blue}%K{black}%}%{%F{yellow}%} ⚡ %{%k%F{black}%}.%{%k%F{blue}%})%{%f%k%b%}'

PROMPT='$(_exit_status)%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
%{$fg_bold[white]%} $SYMBOL_LAMBDA %b%f'
