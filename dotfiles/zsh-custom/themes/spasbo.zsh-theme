
NEWLINE='
'

exists() {
  command -v $1 > /dev/null 2>&1
}

PROMPT_EXIT_CODE_ERROR='✘'
PROMPT_EXIT_CODE_OK='✔'
exit_status() {
  echo " %(?:%{$fg_bold[green]%}$PROMPT_EXIT_CODE_OK:%{$fg_bold[red]%}$PROMPT_EXIT_CODE_ERROR)"
}

SINGLE_SMALL_RIGHT_ARROW='❯'
SYMBOL_LAMBDA="λ"
RIGHT_SEPARATOR=""

# ZSH_THEME_GIT_PROMPT_PREFIX="$fg_bold[blue]Git: %{$fg[green]%}"
# ZSH_THEME_GIT_PROMPT_SUFFIX="$reset_color "

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

ROOT_USER_COLOR='red'
NORMAL_USER_COLOR='blue'

user() {
  if [[ $LOGNAME == $USER ]] || [[ $UID == 0 ]] || [[ -n $SSH_CONNECTION ]]; then
    local user_color
    if [[ $USER == 'root' ]]; then
      user_color=$ROOT_USER_COLOR
    else
      user_color=$NORMAL_USER_COLOR
    fi

  fi

}

function _git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git show-ref --head -s --abbrev |head -n1 2> /dev/null)"
  echo "${ref/refs\/heads\//$GIT_SYMBOL } $(parse_git_dirty)$(git_commits_ahead)$(git_commits_behind)"
#  echo "${ref/refs\/heads\// } $(git_prompt_status)$(git_commits_ahead)$(git_commits_behind)"
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
    echo "%{%K{$BG_COLOR}%}%{%F{$FG_COLOR}%}%{%b%} $(_git_prompt_info) %{%F{$BG_COLOR}%K{blue}%}"
  else
    echo "%{%K{blue}%}"
  fi
}

# PROMPT_HOST='%{%b%F{gray}%K{black}%} %(?.%{%F{green}%}✔.%{%F{red}%}✘) %n%{%F{magenta}%}@%{%F{cyan}%}%m %{%F{black}%}'
PROMPT_HOST='%{%b%f%}%{%F{blue}%} %n%{%F{magenta}%}@%{%F{cyan}%}%m %{%F{black}%}'
PROMPT_DIR='%{%F{white}%} %~%  '
PROMPT_SU='%(!.%{%k%F{blue}%K{black}%}%{%F{yellow}%} ⚡ %{%k%F{black}%}.%{%k%F{blue}%})%{%f%k%b%}'

# PROMPT='$(exit_status)%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU

PROMPT='$(exit_status)%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
%{$fg_bold[white]%} $SYMBOL_LAMBDA %b%f'

####### STOLEN # https://github.com/zakariaGatter/gatter_oh-my-zsh_theme/blob/master/gatter.zsh-theme
# https://github.com/zakariaGatter/Powergate

# local left_status="%(?:%{$fg[green]%}[:%{$fg[red]%}[)"
# local right_status="%(?:%{$fg[green]%}]:%{$fg[red]%}])"

# PROMPT='
# ${left_status}%{$fg_bold[yellow]%} %D %T %{$reset_color%}${right_status} ${left_status}$(git_prompt_info)%{$reset_color%}${right_status}
# ${left_status}%{$fg_bold[cyan]%}%c%{$reset_color%}${right_status}${_user}${ret_status}%{$reset_color%}'
# RPROMPT=$'%b%{$reset_color%}%{$fg_bold[white]%}${${KEYMAP/vicmd/--NORMAL--}/(main|viins)/--INSERT--}%{$reset_color%}'
