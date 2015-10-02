# Yay! High voltage and arrows!

prompt_setup_pygmalion(){
  ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[white]%}"
  ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
  ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[white]%} *%{$reset_color%}"
  ZSH_THEME_GIT_PROMPT_CLEAN=""

  base_prompt='%{$fg_bold[white]%}[%{$reset_color%}%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg_bold[magenta]%}@%{$reset_color%}%{$fg_bold[cyan]%}%m:%{$reset_color%}%{$fg_bold[green]%}%0~%{$reset_color%}%{$fg_bold[white]%}]%{$reset_color%}%{$fg_bold[red]%}|%{$reset_color%}'
  post_prompt='%{$fg_bold[green]%}λ%{$reset_color%} '

  base_prompt_nocolor=$(echo "$base_prompt" | perl -pe "s/%\{[^}]+\}//g")
  post_prompt_nocolor=$(echo "$post_prompt" | perl -pe "s/%\{[^}]+\}//g")

  precmd_functions+=(prompt_pygmalion_precmd)
}

prompt_pygmalion_precmd(){
  local gitinfo=$(git_prompt_info)
  local gitinfo_nocolor=$(echo "$gitinfo" | perl -pe "s/%\{[^}]+\}//g")
  local exp_nocolor="$(print -P \"$base_prompt_nocolor$gitinfo_nocolor$post_prompt_nocolor\")"
  local prompt_length=${#exp_nocolor}

  local nl=""

  if [[ $prompt_length -gt 40 ]]; then
    nl=$'\n%{\r%}';
  fi
#  PROMPT="$base_prompt$gitinfo$nl$post_prompt"
  PROMPT="$base_prompt$gitinfo"$'\n'"$post_prompt"
}

prompt_setup_pygmalion
