
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi



export ANDROID_HOME=/Development/Android/sdk/tools
export PATH=$PATH:/Development/Activator-1.1.3
export PATH=/usr/local/mysql/bin:$PATH
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function
