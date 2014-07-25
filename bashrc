#-------------------------------------------------------------
#           Author: Steven Spasbo
#-------------------------------------------------------------

export DOTFILE_REPO=`dirname (readlink ~/.bashrc)`

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
source $DOTFILE_REPO/git-prompt.sh
export PS1="\[$(tput bold)\]\[$(tput setaf 7)\][\[$(tput setaf 4)\]\h\[$(tput setaf 5)\]@\[$(tput setaf 6)\]\h: \[$(tput setaf 2)\]\w\[$(tput setaf 7)\]]\n\[$(tput setaf 2)\]> \[$(tput sgr0)\]"
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

export RSENSE_HOME=$HOME/.vim/rsense-0.3
export EDITOR=vim

# If you need to add any dirs to path, just add :/path/to/dir after $PATH
#PATH=$PATH

#-------------------------------------------------------------
# Colors
#-------------------------------------------------------------
c0="\033[0m"    # Reset
c1="\033[0;32m" # Green
c2="\033[0;33m" # Yellow
c3="\033[1;31m" # RED
c4="\033[0;31m" # Red
c5="\033[0;35m" # Purple
c6="\033[0;36m" # Blue
blue2="\033[0;34m"
black='\[\e[0;30m\]'
blue='\[\e[0;34m\]'
green='\[\e[0;32m\]'
cyan='\[\e[0;36m\]'
red='\[\e[0;31m\]'
purple='\[\e[0;35m\]'
brown='\[\e[0;33m\]'
lightgray='\[\e[0;37m\]'
darkgray='\[\e[1;30m\]'
lightblue='\[\e[1;34m\]'
lightgreen='\[\e[1;32m\]'
lightcyan='\[\e[1;36m\]'
lightred='\[\e[1;31m\]'
lightpurple='\[\e[1;35m\]'
yellow='\[\e[1;33m\]'
white='\[\e[1;37m\]'
nc='\[\e[0m\]'
cyan2="\033[036m"

#-------------------------------------------------------------
# Functions!!!
#-------------------------------------------------------------
xtitle()
{
  case "$TERM" in
    *term | rxvt)
      echo -ne "\033]0;$*\007" ;;
    *)
      ;;
  esac
}

default_title() 
{
  xtitle `logname` on `hostname`
}

motd()
{
archey -c

}

clear
default_title
motd

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

