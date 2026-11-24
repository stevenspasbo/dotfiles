#!/usr/bin/env zsh

# History options

setopt HIST_IGNORE_ALL_DUPS # Re-running a command removes its older copy (implies HIST_IGNORE_DUPS).
setopt HIST_SAVE_NO_DUPS # Don't write duplicates when the history file is rewritten.
setopt HIST_FIND_NO_DUPS # Don't show the same command twice when searching history.
setopt HIST_REDUCE_BLANKS # Strip extra whitespace so "ls  -l" and "ls -l" count as the same.
setopt HIST_IGNORE_SPACE # If a command starts with a space, it will not be written to $HISTFILE.
setopt SHARE_HISTORY # Write commands immediately and import new ones from other sessions.
                     # (Includes INC_APPEND_HISTORY's behavior; don't set both.)

unsetopt HIST_VERIFY
unsetopt EXTENDED_HISTORY # Don't save timestamp info with history