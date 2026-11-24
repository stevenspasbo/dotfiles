#!/usr/bin/env zsh

# zsh-autosuggestions settings. Read at suggestion time, so setting them after
# the plugin loads is fine.

# Suggest from history first; fall back to tab completion when nothing matches.
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# Stop suggesting once the line is longer than this (e.g. pasted text), which
# can lag. Note: this disables suggestions for ANY line over this length.
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=150
