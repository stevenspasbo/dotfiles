#-------------------------------------------------------------
#           Author: Steven Spasbo
#-------------------------------------------------------------
if [[ -L ~/.bashrc ]]; then
  export DOTFILE_REPO=`dirname $(readlink ~/.bashrc)`
  source $DOTFILE_REPO/git-prompt.sh
fi

#-------------------------------------------------------------
# History stuff
#-------------------------------------------------------------
export HISTCONTROL=ignoreboth:ignoredups:erasedups # Ignore lines that are duplicates or begin with space
export HISTSIZE=10000
export HISTFILESIZE=100000
export HISTFILE=~/.bash_history
shopt -s histappend # Append to history file instead of overwriting
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

#-------------------------------------------------------------
# Prompt
#-------------------------------------------------------------
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\[$(tput bold)\]\[$(tput setaf 7)\][\[$(tput setaf 4)\]\h\[$(tput setaf 5)\]@\[$(tput setaf 6)\]\h: \[$(tput setaf 2)\]\w\[$(tput setaf 7)\]]$(__git_ps1 " (%s)")\n\[$(tput setaf 2)\]> \[$(tput sgr0)\]'
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

#-------------------------------------------------------------
# Environment stuff
#-------------------------------------------------------------
LC_CTYPE=en_US.UTF-8

# Load bash_aliases file
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi
if [ -f ~/.profile ]; then
    source ~/.profile
fi

export EDITOR=vim

# If you need to add any dirs to path, just add :/path/to/dir after $PATH
#PATH=$PATH

#-------------------------------------------------------------
# Functions
#-------------------------------------------------------------

motd() {
  archey -c
}

clear
motd


