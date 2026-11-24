#!/usr/bin/env zsh

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

bindkey "^[h" run-help
bindkey "^Xb" describe-key-briefly
