
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

    if ! command -v brew > /dev/null; then
      /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi

    PACKAGE_MANAGER='brew'

    ;;
  *) ;;
esac

if command > git; then
  if [[ ! -d "$HOME/dotfiles" ]]; then
    git clone --recursive https://github.com/stevenspasbo/dotfiles.git "$HOME/dotfiles"
  fi
fi
