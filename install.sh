
case "$OSTYPE" in
  darwin*)  echo "OSX" ;;
  linux*)   echo "LINUX" ;;
  *)        echo "unknown: $OSTYPE" ;;
esac

DOTFILES_DIR="$HOME/dotfiles"

if command > git; then
  if [[ ! -d "$DOTFILES_DIR" ]]; then
    echo "Checking out dotfiles repository..."
    git clone --recursive https://github.com/stevenspasbo/dotfiles.git "$DOTFILES_DIR" > /dev/null
  fi
fi

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

    cd "$DOTFILES_DIR"
    brew bundle

    ;;
  *) ;;
esac
