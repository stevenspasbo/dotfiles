# Original theme https://github.com/agnoster zsh theme

################################################################
# Symbols
################################################################

# Powerline
SYM_SOLID_LEFT_ARROW='\ue0b2' # 
SYM_OPEN_LEFT_ARROW='\ue0b3' # 
SYM_OPEN_RIGHT_ARROW='\ue0b1' # 
SYM_SOLID_RIGHT_ARROW='\ue0b0' # 

# Git
SYM_CHECKMARK='\u2714' # ✔
SYM_CHECKMARK='\u2713' # ✓
SYM_PLUS_MINUS='\u00b1' # ±
SYM_X='\u2718' # ✘
SYM_BIG_X='\u2716' # ✖
SYM_GIT_BRANCH='\ue0a0' # 
SYM_DETACHED_HEAD='\u27a6' # ➦

SYM_DIRTY='●'
SYM_ADD='✚'

# Misc
SYM_LAMBDA='\u03bb' # λ
SYM_CLOUD='\u2601' # ☁
SYM_OMEGA='\u3a9' # Ω
SYM_SIGMA='\u2211' # ∑
SYM_LIGHTNING='\u26a1' # ⚡
SYM_LN="\xee\x82\xa1" # 
SYM_LOCK="\xee\x82\xa2" # 
SYM_DOUBLE_RIGHT='\u00bb' # »
SYM_ROUND_ARROW='\u21ba' # ↺
SYM_DOWN_ARROW='\u2193' # ↓
SYM_UP_ARROW='\u2191' # ↑

#########

ZSH_THEME_GIT_PROMPT_DIRTY=$SYM_DIRTY




function _git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git show-ref --head -s --abbrev |head -n1 2> /dev/null)"
  echo "${ref/refs\/heads\// }$(parse_git_dirty)"
}

function _git_info() {
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    local BG_COLOR=green
    if [[ -n $(parse_git_dirty) ]]; then
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

PROMPT_HOST='%{%b%F{gray}%K{black}%} %(?.%{%F{green}%}✔.%{%F{red}%}✘)%{%F{yellow}%} %n %{%F{black}%}'
PROMPT_DIR='%{%F{white}%} %~%  '
PROMPT_SU='%(!.%{%k%F{blue}%K{black}%}%{%F{yellow}%} ⚡ %{%k%F{black}%}.%{%k%F{blue}%})%{%f%k%b%}'

SYM_SINGLE_SMALL_ARROW='❯'

PROMPT='%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
%{$fg_bold[white]%} λ %b%f'
# RPROMPT='%{$fg[green]%}[%*]%{$reset_color%}'
