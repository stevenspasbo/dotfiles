
# ------------------------------------------------------------------------------
# Zsh Theme
#
# A custom, Powerline-style theme with segments for exit status, host info,
# Git status, and current directory.
# ------------------------------------------------------------------------------

# Ensure Zsh's color system is loaded.
autoload -U colors && colors

# --- Dependency Check ---
# This theme requires functions from Oh My Zsh's 'git.zsh' library.
# Ensure it is sourced before this theme is loaded.
if ! command -v parse_git_dirty &> /dev/null; then
  echo "Zsh Theme Error: Git prompt functions not found."
  echo "Please source 'lib/git.zsh' from Oh My Zsh."
  return 1
fi

# ------------------------------------------------------------------------------
# Symbols & Icons
# ------------------------------------------------------------------------------
# A powerline font is required.

PROMPT_EXIT_CODE_OK='✔'
PROMPT_EXIT_CODE_ERROR='✘'

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

# Prompt characters
local a_prompt_char_ok="✔"
local a_prompt_char_error="✘"
local a_prompt_char_root="⚡"
local a_prompt_char_user="❯"
local a_prompt_char_lambda="λ"

# Powerline separators
local a_sep_right=""
local a_sep_left=""

# Git symbols
local a_git_branch_icon=""


# ------------------------------------------------------------------------------
# Prompt Segments
#
# Each function builds a discrete part of the prompt.
# ------------------------------------------------------------------------------

# Segment 1: Exit code of the last command.
prompt_exit_code() {
  # Shows OK symbol in green for success, ERROR symbol in red for failure.
  prompt_segment black default "%(?.%{$fg[green]%}$a_prompt_char_ok.%{$fg[red]%}$a_prompt_char_error)"
}

# Segment 2: User and host information.
prompt_host() {
  # Format: user@hostname
  prompt_segment blue default " %n%{$fg[magenta]%}@%{$fg[cyan]%}%m "
}

# Segment 3: Git repository status.
prompt_git() {
  # Only shows if inside a Git repository.
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    return # Not a git repo, do nothing.
  fi

  # Determine colors based on repo status.
  local bg_color="green"
  local fg_color="black"
  if [[ -n "$(git status --short 2> /dev/null)" ]]; then
    bg_color="yellow" # Dirty repo
  fi
  if [[ -n "$(git ls-files --other --exclude-standard 2> /dev/null)" ]]; then
    bg_color="red" # Untracked files (highest priority)
    fg_color="white"
  fi

  # Get the detailed Git status string.
  # Relies on Oh My Zsh's git functions.
  local git_details
  git_details+="$a_git_branch_icon $(git_current_branch)"
  git_details+="$(parse_git_dirty)"
  git_details+="$(git_commits_ahead)"
  git_details+="$(git_commits_behind)"

  # Print the formatted segment with separators.
  prompt_segment "$bg_color" blue "$a_sep_right" # Separator from host -> git
  prompt_segment "$fg_color" "$bg_color" " $git_details " # Git text
  prompt_segment "$bg_color" default "$a_sep_right" # Separator from git -> dir
}

function _git_prompt_info() {
  local git_ref
  local branch_string

  git_ref=$(git symbolic-ref HEAD 2>/dev/null) \
    || git_ref="➦ $(git show-ref --head -s --abbrev | head -n1 2>/dev/null)"

  # Exit if we're not in a Git repo.
  if [[ -z "$git_ref" ]]; then
    return
  fi

  branch_string="${git_ref/refs\/heads\//$GIT_SYMBOL }"
  # Truncate the branch name if it's too long (e.g., feature/TICKET-123-a-very-long-name).
  # The '(r.40.)' flag truncates from the left, keeping the end of the name visible.
  if (( ${#branch_string} > 40 )); then
    branch_string="${(r.40.)${branch_string}}"
  fi

  # Assemble the final output string.
  echo "${branch_string} $(parse_git_dirty)$(git_commits_ahead)$(git_commits_behind)"
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


NEWLINE=$'\n'

PROMPT=""

# PROMPT+=" $(prompt_exit_code)"
PROMPT+="$(_exit_status)%{%f%b%k%}"

PROMPT+="$PROMPT_HOST"

PROMPT+="$(_git_info)"

PROMPT+="$PROMPT_DIR"

PROMPT+="$PROMPT_SU"


PROMPT+="${NEWLINE}"
PROMPT+="%{$fg_bold[white]%} $SYMBOL_LAMBDA %b%f"



PROMPT='$(_exit_status)%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
%{$fg_bold[white]%} $SYMBOL_LAMBDA %b%f'




# Assemble the main prompt (PROMPT) by calling segment functions.
# PROMPT=""
# PROMPT+="$(prompt_exit_code)"
# PROMPT+="$(prompt_host)"
# PROMPT+="$(prompt_git)"
# PROMPT+="$(prompt_dir)"
# PROMPT+="$(prompt_root)"
# PROMPT+=$'\n' # Newline for the command area
# PROMPT+="%{$fg_bold[white]%}$a_prompt_char_lambda %{$reset_color%}"


# Right-side prompt (RPROMPT) can be defined here if needed.
RPROMPT=""