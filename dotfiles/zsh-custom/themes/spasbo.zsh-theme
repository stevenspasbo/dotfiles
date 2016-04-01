prompt_spasbo_precmd(){
    if git rev-parse --git-dir > /dev/null 2>&1; then

        ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%} ("
        ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}✓%{$reset_color%}"
        ZSH_THEME_GIT_PROMPT_STAGED="%{$fg_bold[green]%}+"
        # ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[green]%}✚ "
        # ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg[yellow]%}*"
        ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg[yellow]%}●"
        ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[magenta]%}?"
        ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%}✖"
        ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[white]%})%{$reset_color%} "

        local _GIT_DIR_STATUS=$(command git status --porcelain -b 2> /dev/null)
        local ahead=$(git_commits_ahead 2> /dev/null)
        local behind=$(git_commits_behind 2> /dev/null)
        local branch=$(git_current_branch)
        local _STATUS="$ZSH_THEME_GIT_PROMPT_PREFIX$branch"
        # Commits behind
        if [[ $behind -ne "0" ]]; then
	    ZSH_THEME_GIT_PROMPT_BEHIND="$behind↓"
	    _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_BEHIND
        fi
        # Commits ahead
        if [[ $ahead -ne "0" ]]; then
	    ZSH_THEME_GIT_PROMPT_AHEAD="$ahead↑"
	    _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_AHEAD
        fi
        _STATUS="$_STATUS|"
        # Unstaged
        if $(echo "$_GIT_DIR_STATUS" | grep '^.[MT] ' &> /dev/null); then
	    _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_UNSTAGED
        fi
        # Staged
        if $(echo "$_GIT_DIR_STATUS" | grep '^[AMR]. ' &> /dev/null); then
	    _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_STAGED
        fi
        # Deleted
        if $(echo "$_GIT_DIR_STATUS" | grep '^.[D] ' &> /dev/null); then
	    _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_DELETED
        fi
        # Untracked
        if $(echo "$_GIT_DIR_STATUS" | grep -E '^\?\? ' &> /dev/null); then
	    _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_UNTRACKED
        fi
        # Looks clean
        if [[ $(echo "$_GIT_DIR_STATUS" | wc -l) -eq 1 ]]; then
            _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_CLEAN
        fi

        _STATUS=$_STATUS$ZSH_THEME_GIT_PROMPT_SUFFIX
    fi
    PROMPT="$base_prompt$_STATUS"$'\n'"$post_prompt"
}

prompt_setup_spasbo() {
    base_prompt='%{$fg_bold[white]%}[%{$reset_color%}%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg_bold[magenta]%}@%{$reset_color%}%{$fg_bold[cyan]%}%m:%{$reset_color%}%{$fg_bold[green]%}%0~%{$reset_color%}%{$fg_bold[white]%}]%{$reset_color%}'
    post_prompt='%{$fg_bold[green]%}λ%{$reset_color%} '

    base_prompt_nocolor=$(echo "$base_prompt" | perl -pe "s/%\{[^}]+\}//g")
    post_prompt_nocolor=$(echo "$post_prompt" | perl -pe "s/%\{[^}]+\}//g")

    if [[ \
           ${precmd_functions[(r)prompt_spasbo_precmd]} != prompt_spasbo_precmd \
       ]];
    then;
        precmd_functions+=(prompt_spasbo_precmd);
    fi
}

prompt_setup_spasbo
