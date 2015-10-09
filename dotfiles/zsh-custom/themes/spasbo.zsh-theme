
prompt_spasbo_precmd(){
if git rev-parse --git-dir > /dev/null 2>&1; then

    # (master↓2↑3|*+?)
    local _GIT_DIR_STATUS=$(command git status --porcelain -b 2> /dev/null)
    local ahead=$(git_commits_ahead 2> /dev/null)
    local behind=$(git_commits_behind 2> /dev/null)

    local _STATUS=""

    ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%} ("
    if [[ $behind -ne "0" ]]; then
	ZSH_THEME_GIT_PROMPT_BEHIND="↓$behind"
	_STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_BEHIND
    fi;
    if [[ $ahead -ne "0" ]]; then
	ZSH_THEME_GIT_PROMPT_AHEAD="↑$ahead"
	_STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_AHEAD|"
    else
	_STATUS="$_STATUS|"
    fi
    ZSH_THEME_GIT_PROMPT_CLEAN="|%{$fg_bold[green]%}✓%{$reset_color%}"
    if $(echo "$_GIT_DIR_STATUS" | grep '^[AMRD]. ' &> /dev/null); then
	#    ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[green]%}✚ "
	ZSH_THEME_GIT_PROMPT_ADDED="%{$fg_bold[green]%}+"
	_STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_ADDED
    fi
    if $(echo "$_GIT_DIR_STATUS" | grep '^.[MTD] ' &> /dev/null); then
	ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg[yellow]%}*"
	_STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_UNSTAGED
    fi
    if $(echo "$_GIT_DIR_STATUS" | grep -E '^\?\? ' &> /dev/null); then
	ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[magenta]%}?"
	_STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_UNTRACKED
    fi
    ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%}●"

    ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[white]%})%{$reset_color%} "
    ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%}✖"
    #local behindahead=$ZSH_THEME_GIT_PROMPT_BEHIND$ZSH_THEME_GIT_PROMPT_AHEAD

    ZSH_THEME_GIT_PROMPT_DIRTY="$_STATUS%{$reset_color%}"

    local gitinfo=$(git_prompt_info)
    local gitinfo_nocolor=$(echo "$gitinfo" | perl -pe "s/%\{[^}]+\}//g")

fi
    local exp_nocolor="$(print -P \"$base_prompt_nocolor$gitinfo_nocolor$post_prompt_nocolor\")"
    local prompt_length=${#exp_nocolor}

    PROMPT="$base_prompt$gitinfo"$'\n'"$post_prompt"
}

prompt_setup_spasbo(){
  base_prompt='%{$fg_bold[white]%}[%{$reset_color%}%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg_bold[magenta]%}@%{$reset_color%}%{$fg_bold[cyan]%}%m:%{$reset_color%}%{$fg_bold[green]%}%0~%{$reset_color%}%{$fg_bold[white]%}]%{$reset_color%}'
  post_prompt='%{$fg_bold[green]%}λ%{$reset_color%} '

  base_prompt_nocolor=$(echo "$base_prompt" | perl -pe "s/%\{[^}]+\}//g")
  post_prompt_nocolor=$(echo "$post_prompt" | perl -pe "s/%\{[^}]+\}//g")

  precmd_functions+=(prompt_spasbo_precmd)
}

prompt_setup_spasbo
