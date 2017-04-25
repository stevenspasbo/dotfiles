#!/usr/bin/env zsh

autoload run-help # Open man pages with `command <esc-h>`

bindkey '^H' run-help

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
