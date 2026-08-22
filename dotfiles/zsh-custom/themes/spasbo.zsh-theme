

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


function _git_prompt_info() {
  local ref branch
  ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git show-ref --head -s --abbrev |head -n1 2> /dev/null)"

  branch="${ref/refs\/heads\//$GIT_SYMBOL }"
  # Truncate long branch names to 40 chars; '(r.40.)' pads/truncates from the left,
  # keeping the end of the name visible.
  if (( ${#branch} > 40 )); then
    branch="${(r.40.)${branch}}"
  fi

  echo "$branch $(parse_git_dirty)$(git_commits_ahead)$(git_commits_behind)"
}

function _git_info() {
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    local BG_COLOR=green
    if [[ -n "$(git status --short)" ]]; then
      BG_COLOR=yellow
      FG_COLOR=black
    fi

    if [[ ! -z $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
        BG_COLOR=red
        FG_COLOR=white
    fi
    echo "%{%K{$BG_COLOR}%}%{%F{$FG_COLOR}%} $(_git_prompt_info) %{%F{$BG_COLOR}%K{blue}%}"
  else
    echo "%{%K{blue}%}"
  fi
}

PROMPT_HOST='%{%b%f%}%{%F{blue}%} %n%{%F{magenta}%}@%{%F{cyan}%}%m %{%F{black}%}'
PROMPT_DIR='%{%F{white}%} %~%  '
PROMPT_SU='%(!.%{%k%F{blue}%K{black}%}%{%F{yellow}%} ⚡ %{%k%F{black}%}.%{%k%F{blue}%})%{%f%k%b%}'

PROMPT='$(_exit_status)%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
%{$fg_bold[white]%} $SYMBOL_LAMBDA %b%f'
