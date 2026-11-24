#!/usr/bin/env zsh

# run-help is autoloaded in zshrc (HELPDIR block).

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

bindkey "^[h" run-help
bindkey "^Xb" describe-key-briefly
