#-------------------------------------------------------------
#           Author: Steven Spasbo
#-------------------------------------------------------------
alias a='alias'

#-------------------------------------------------------------
# grep stuff
#-------------------------------------------------------------
a grep='grep --color'
a fgrep='fgrep --color'
a egrep='egrep --color'

#-------------------------------------------------------------
# ls stuff
#-------------------------------------------------------------
a ls='ls'
a ll="ls -la"
a l='ls -CF'

#-------------------------------------------------------------
# System stuff
#-------------------------------------------------------------
a reload='clear && source ~/.bashrc'
a hist='history | grep'
a cls='clear'
a cal='cal | grep --before-context 6 --after-context 6 --color -e " $(date +%e)" -e "^$(date +%e)"'

a ping='ping -c 5'

#-------------------------------------------------------------
# Shortcuts and directory stuff
#-------------------------------------------------------------
a ..='cd ..'
a ...='cd ../..'
a home='cd ~/'
a downloads='cd ~/Downloads'
a desktop='cd ~/Desktop'

#-------------------------------------------------------------
# Personalized shortcuts
#-------------------------------------------------------------
a ru="cd /Development/Ruby"
a py="cd /Development/Python"
a ja="cd /Development/Java"

a chbash="vim ~/.bashrc"
a chvim="vim ~/.vimrc"

#-------------------------------------------------------------
# Version control stuff
#-------------------------------------------------------------

# Blank for now

#-------------------------------------------------------------
# Functions
#-------------------------------------------------------------

# VARIABLES

function setjdk() {  
  if [ $# -ne 0 ]; then  
    removeFromPath '/System/Library/Frameworks/JavaVM.framework/Home/bin'  
    if [ -n "${JAVA_HOME+x}" ]; then  
      removeFromPath $JAVA_HOME  
    fi  
    export JAVA_HOME=`/usr/libexec/java_home -v $@`  
    export PATH=$JAVA_HOME/bin:$PATH  
  fi  
}  
function removeFromPath() {  
  export PATH=$(echo $PATH | sed -E -e "s;:$1;;" -e "s;$1:?;;")  
}

#-------------------------------------------------------------
# Device specific functions
#-------------------------------------------------------------

# Personal laptop
if [ $(hostname) == "h4xx0rmaxx0rzzzz.local" ]; then

fi

# Work laptop
if [ $(hostname) == "SSPASBO-03.local" ]; then
  setjdk 1.7

  function syncpdfs() {
    rsync -avz /Users/steven.spasbo/Documents/PDFs steven@stevenspasbo.com:/home/steven/PDFs
  }
  
  repos=( "/Work/SVN/auto" )
  updaterepos() {
  for i in "${repos[@]}"
  do 
    echo "Updating $i"
    svn up $i
    echo ""
  done
  }
  
  AUTO="/Work/SVN/auto"
  ssh-suv() {
    scp-profile $1
    echo "ssh to $1"
    ssh $1 -A
  }
  
  scp-profile() {
    echo "Sending .bash_profile to ~/ on $1"
    scp $AUTO/trunk/wats/BashScript/.bash_profile root@$1:~/.bash_profile
    echo "Sending .vimrc to ~/ on $1"
    scp ~/.vimrc root@$1:~/.vimrc
    echo "Sending vim plugins"
    rsync -a --ignore-existing --exclude="*/.git/" ~/.vim/ root@$1:~/.vim
    echo "Sending my.wats.properties"
    scp ~/.my.wats.properties root@$1:/data/workdaydevqa/suv/suvwats/my.wats.properties
    echo "Done, logging in"
  }

  getlogs() {
  WATSLOGS="/Work/Logs/WATS"
  CURRENTDIRECTORY=`pwd`
  DATE="$(date +%Y-%m-%d-%H_%M_%S)"
  REMOTELOGS="/data/workdaydevqa/suv/suvwats/logs"
  if [ -z $1 ]; then
    echo "Please enter an SUV"
  else
    if [ -e $WATSLOGS ]; then
      echo "Getting logs"
      cd $WATSLOGS && mkdir $DATE && cd $DATE
      scp root@$1:"$REMOTELOGS/*.html $REMOTELOGS/*.log $REMOTELOGS/*.xml" ./
      echo "Done" && cd $CURRENTDIRECTORY
    fi
  fi
  }
fi

