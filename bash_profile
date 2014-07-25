
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export ANDROID_HOME=/Development/Android/sdk/tools
export PATH=$PATH:/Development/Activator-1.1.3
export PATH=/usr/local/mysql/bin:$PATH

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function
