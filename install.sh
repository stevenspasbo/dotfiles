
case "$OSTYPE" in
  darwin*)  echo "OSX" ;;
  linux*)   echo "LINUX" ;;
  *)        echo "unknown: $OSTYPE" ;;
esac

# Detect the platform (similar to $OSTYPE)
OS="`uname`"
case $OS in
  'Linux')
    IS_LINUX=true
    PACKAGE_MANAGER='sudo apt-get'
    ;;
  'Darwin')
    IS_MAC=true

    if ! command -v brew 2>&1 /dev/null; then
      /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi

    PACKAGE_MANAGER='brew'

    ;;
  *) ;;
esac


################################################################
# NVM
################################################################
# Install node

[ ! -d $HOME/.nvm ] && git clone https://github.com/creationix/nvm.git ~/.nvm && sh ~/.nvm/install.sh
. $NVM_DIR/nvm.sh && nvm install stable
npm install -g grunt-cli

# gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
# \curl -sSL https://get.rvm.io | bash -s stable
