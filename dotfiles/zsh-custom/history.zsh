#!/usr/bin/env zsh

# History options

setopt HIST_EXPIRE_DUPS_FIRST # Removes older duplicate entries first.
setopt HIST_IGNORE_DUPS # Prevents duplicate entries.
setopt HIST_IGNORE_SPACE # If a command starts with a space, it will not be written to $HISTFILE.
setopt INC_APPEND_HISTORY # Allows multiple sessions to write to $HISTFILE in parallel.

unsetopt HIST_VERIFY
unsetopt EXTENDED_HISTORY # Don't save timestamp info with history