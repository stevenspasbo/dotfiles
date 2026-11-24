#!/usr/bin/env zsh

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
ZSH_HIGHLIGHT_MAXLENGTH=300


HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=magenta,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'

# Colors for token types that are uncolored by default
typeset -gA ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=green'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[default]='fg=white'

# : ${ZSH_HIGHLIGHT_STYLES[alias]:=fg=blue}
# : ${ZSH_HIGHLIGHT_STYLES[builtin]:=fg=141}
# : ${ZSH_HIGHLIGHT_STYLES[globbing]:=fg=105}
