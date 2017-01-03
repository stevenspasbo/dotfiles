#!/usr/bin/env zsh

# History stuff

if [ -z "$HISTFILE" ]; then
    HISTFILE="$HOME/.history"
fi
export HISTSIZE=100000
export SAVEHIST=100000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
