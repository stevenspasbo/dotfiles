
# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max -1 # Never forget

zstyle ':completion:*' completer _complete _ignored _approximate
# Speeds up path completions
zstyle ':completion:*' accept-exact '*(N)'
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
zstyle ':completion:*:aliases' list-colors "=*=$color[blue]"

# Enable approximate completions
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3)) numeric)'

# Always use menu selection for `cd -`
zstyle ':completion:*:*:cd:*:directory-stack' force-list always
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# Enable menus
zstyle ':completion:*' menu select
# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
# Group items by group
zstyle ':completion:*' group-name ''
# Verbose completion results
zstyle ':completion:*' extra-verbose true
# Results; Sets formatting of group titles
zstyle ':completion:*:descriptions' format '%F{yellow}%B---- %d%b'
# No results; ex: commandthatdoesntexist[tab]
zstyle ':completion:*:warnings' format '%F{red%}%BNo results in: %ds%b%f'
# Autocorrect
zstyle ':completion:*:corrections' format '%U%F{green}%d%f%u'
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'

zstyle ':completion:*:processes' menu yes select
zstyle ':completion:*:processes' force-list always
zstyle ':completion:*:processes'             command 'ps -axw'
zstyle ':completion:*:processes-names'       command 'ps -awxho command'
zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]#) ([\.0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,args -w -w"

# kill command completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#) #([^ ]#)*=$color[cyan]=$color[yellow]=$color[green]"

# make "rm [tab]" look like ls -l
zstyle ':completion:*:*:rm:*' file-list 'yes'

# Ignore files already in active line
zstyle ':completion::*:(git|less|rm|vim|most)' ignore-line true

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' rehash true

# Ignore missing commands
zstyle ':completion:*:functions' ignored-patterns '_*'
