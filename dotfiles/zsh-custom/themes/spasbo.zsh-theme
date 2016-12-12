ZSH_THEME_GIT_PROMPT_UNTRACKED="?"
ZSH_THEME_GIT_PROMPT_ADDED='✚'
ZSH_THEME_GIT_PROMPT_MODIFIED='●'
ZSH_THEME_GIT_PROMPT_RENAMED='↺'
ZSH_THEME_GIT_PROMPT_DELETED='✘'

ZSH_THEME_GIT_PROMPT_DIRTY='●'
ZSH_THEME_GIT_PROMPT_CLEAN='✔'
ZSH_THEME_GIT_COMMITS_BEHIND_SUFFIX='↓'
# Gives a little breathing room between status and commit diffs.
ZSH_THEME_GIT_COMMITS_AHEAD_PREFIX=' '
ZSH_THEME_GIT_COMMITS_AHEAD_SUFFIX='↑'

function _git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git show-ref --head -s --abbrev |head -n1 2> /dev/null)"
  echo "${ref/refs\/heads\// } $(git_prompt_status)$(git_commits_ahead)$(git_commits_behind)"
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

PROMPT_HOST='%{%b%F{gray}%K{black}%} %(?.%{%F{green}%}✔.%{%F{red}%}✘)%{%F{yellow}%} %{%F{blue}%} %n%{%F{magenta}%}@%{%F{cyan}%}%m %{%F{black}%}'
PROMPT_DIR='%{%F{white}%} %~%  '
PROMPT_SU='%(!.%{%k%F{blue}%K{black}%}%{%F{yellow}%} ⚡ %{%k%F{black}%}.%{%k%F{blue}%})%{%f%k%b%}'

SYM_SINGLE_SMALL_ARROW='❯'

PROMPT='%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
%{$fg_bold[white]%} λ %b%f'
# RPROMPT='%{$fg[green]%}[%*]%{$reset_color%}'
