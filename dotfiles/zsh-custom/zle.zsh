# Show dots when completing.
expand-or-complete-with-dots() {
  echo -n "\e[31m......\e[0m"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

# Token types styles for ZLE syntax highlighting.
# See http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#SEC135 for more details.
: ${ZLE_RESERVED_WORD_STYLE:='fg=yellow'}
: ${ZLE_ALIAS_STYLE:='fg=magenta'}
: ${ZLE_BUILTIN_STYLE:='fg=cyan'}
: ${ZLE_FUNCTION_STYLE:='fg=blue'}
: ${ZLE_COMMAND_STYLE:='fg=green'}
: ${ZLE_COMMAND_UNKNOWN_TOKEN_STYLE:='fg=red'}
: ${ZLE_HYPHEN_CLI_OPTION_STYLE:='fg=yellow'}
: ${ZLE_DOUBLE_HYPHEN_CLI_OPTION_STYLE:='fg=green'}
: ${ZLE_SINGLE_QUOTED_STYLE:='fg=magenta'}
: ${ZLE_DOUBLE_QUOTED_STYLE:='fg=red'}
: ${ZLE_BACK_QUOTED_STYLE:='fg=cyan'}
: ${ZLE_GLOBBING_STYLE:='fg=blue'}
: ${ZLE_DEFAULT_STYLE:='fg=white'}

# Commands that are typically followed by other commands.
ZLE_TOKENS_FOLLOWED_BY_COMMANDS=('|' '||' ';' '&' '&&' 'sudo' 'start' 'time' 'strace' '§')

# Recolorize the current ZLE buffer.
colorize-zle-buffer() {
  region_highlight=()
  local colorize=true
  local start_pos=0
  local res
  local style
  for arg in ${(z)BUFFER}; do
    ((start_pos += ${#BUFFER[$start_pos+1,-1]} - ${#${BUFFER[$start_pos+1,-1]## #}}))
    local end_pos=$((start_pos + ${#arg}))
    if $colorize; then
      colorize=false
      res=$(LC_ALL=C builtin type $arg 2>/dev/null)
      case $res in
        *'reserved word'*) style=$ZLE_RESERVED_WORD_STYLE;;
        *'an alias'*)      style=$ZLE_ALIAS_STYLE;;
        *'shell builtin'*)  style=$ZLE_BUILTIN_STYLE;;
        *'shell function'*) style=$ZLE_FUNCTION_STYLE;;
        *"$arg is"*)       style=$ZLE_COMMAND_STYLE;;
        *)                 style=$ZLE_COMMAND_UNKNOWN_TOKEN_STYLE;;
      esac
    else
      case $arg in
        '--'*)     style=$ZLE_DOUBLE_HYPHEN_CLI_OPTION_STYLE;;
        '-'*)      style=$ZLE_HYPHEN_CLI_OPTION_STYLE;;
        "'"*"'")   style=$ZLE_SINGLE_QUOTED_STYLE;;
        '"'*'"')   style=$ZLE_DOUBLE_QUOTED_STYLE;;
        '`'*'`')   style=$ZLE_BACK_QUOTED_STYLE;;
        *"*"*)     style=$ZLE_GLOBBING_STYLE;;
        *)         style=$ZLE_DEFAULT_STYLE;;
      esac
    fi
    region_highlight+=("$start_pos $end_pos $style")
    [[ ${${ZLE_TOKENS_FOLLOWED_BY_COMMANDS[(r)${arg//|/\|}]}:+yes} = 'yes' ]] && colorize=true
    start_pos=$end_pos
  done
}

# Bind the function to ZLE events to trigger recoloring.
ZLE_COLORED_FUNCTIONS=(
  self-insert
  delete-char
  backward-delete-char
  kill-word
  backward-kill-word
  up-line-or-history
  down-line-or-history
  beginning-of-history
  end-of-history
  undo
  redo
  yank
)

for f in $ZLE_COLORED_FUNCTIONS; do
  eval "$f() { zle .$f && colorize-zle-buffer } ; zle -N $f"
done

# Expand or complete hack
# Thanks to James Ahlborn.
# This creates an expansion widget that mimics the original "expand-or-complete".
# (You can see the default setup using "zle -l -L").
zle -C orig-expand-or-complete .expand-or-complete _main_complete

# Use the orig-expand-or-complete inside the colorize function. For some reason,
# using the ".expand-or-complete" widget doesn't work the same.
expand-or-complete() {
  builtin zle orig-expand-or-complete && colorize-zle-buffer
}
zle -N expand-or-complete
