#!/usr/bin/env zsh

# Completion stuff
# For good examples, check out http://zshwiki.org/home/examples/compquickstart

zmodload -i zsh/complist

# If we have pip3 installed, make sure it uses pip's completions.
if command -v pip3 > /dev/null; then
  compdef pip3=pip
fi

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max -1 # Never forget

# vcs_info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' disable bzr cdv cvs darcs fossil mtn p4 svk tla

# Rehash installed applications
zstyle ':completion:*' rehash true

# Completion style
zstyle ':completion:*' completer _complete _ignored _approximate
# Speeds up path completions
zstyle ':completion:*' accept-exact '*(N)'

#
# Headers
#
# Results; Sets formatting of group titles
zstyle ':completion:*:descriptions' format '%F{yellow}%B---- %d%b'
# No results; ex: commandthatdoesntexist[tab]
zstyle ':completion:*:warnings' format '%F{red%}%BNo results in: %ds%b%f'
# Suggested corrections
zstyle ':completion:*:corrections' format '%U%F{green}%d%f%u'

zstyle ':completion:*:messages' format '%B%U---- %d%u%b'

# Enable menus
zstyle ':completion:*' menu select

#
# Items
#
# Describe options in full
zstyle ':completion:*:options' description yes
zstyle ':completion:*:options' auto-description '%d'
# Group items by group
zstyle ':completion:*' group-name ''

zstyle ':completion:*' separate-sections 'yes'
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*' file-sort name

# Pretty messages during pagination
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
# show a screenful of options + prompt
COMPLETION_PROMPT="%{$fg_bold[white]%}%M"
COMPLETION_PROMPT+="%{$fg_bold[blue]%}["
COMPLETION_PROMPT+="%{$fg[yellow]%}%P"
COMPLETION_PROMPT+="%{$fg_bold[blue]%}]"
COMPLETION_PROMPT+="%{$fg[white]%}> :%{$reset_color%}"
zstyle ':completion:*' list-prompt $COMPLETION_PROMPT

# Color aliases
#zstyle ':completion:*:aliases' list-colors "=*=$color[blue]"
# Color parameters
#zstyle ':completion:*:parameters' list-colors "=*=32"
# Use ls colors
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# Highlights matching part of command
zstyle -e ':completion:*:-command-:*:commands' list-colors 'reply=( '\''=(#b)('\''$words[CURRENT]'\''|)*-- #(*)=0=38;5;45=38;5;136'\'' '\''=(#b)('\''$words[CURRENT]'\''|)*=0=38;5;45'\'' )'



# Color reserved words
zstyle ':completion:*:reserved-words' list-colors "=*=$color[red]"

zstyle ':completion:*:original' list-colors "=*=$color[red];$color[bold]"

zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Enable approximate completions
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3)) numeric)'

# Always use menu selection for `cd -`
zstyle ':completion:*:*:cd:*:directory-stack' force-list always
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select


# Verbose completion results
zstyle ':completion:*' extra-verbose true
# make them a little less short, after all (mostly adds -l option to the whatis calll)
zstyle ':completion:*:command-descriptions' command '_call_whatis -l -s 1 -r .\*; _call_whatis -l -s 6 -r .\* 2>/dev/null'

zstyle ':completion:*:processes' menu yes select
zstyle ':completion:*:processes' force-list always
zstyle ':completion:*:processes'             command 'ps -axw'
zstyle ':completion:*:processes-names'       command 'ps -awxho command'
zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]#) ([\.0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,args -w -w"

# kill command completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#) #([^ ]#)*=$color[cyan]=$color[red]=$color[green]"

# make "rm [tab]" look like ls -l
zstyle ':completion:*:*:rm:*' file-list 'yes'

# Ignore files already in active line
zstyle ':completion::*:(git|less|rm|vim|most)' ignore-line true

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Ignore missing commands
zstyle ':completion:*:functions' ignored-patterns '_*'
