#! /usr/bin/env bash

# First things first, check for git. We're gonna need it.
if ! command -v git > /dev/null; then
  echo "Install git first."
  exit -1
fi

STARTING_DIRECTORY="$PWD"
cd "$HOME"

# We'll need files out of the repo, make sure they're there.
DOTFILES_DIR="$HOME/dotfiles"
if [[ ! -d "$DOTFILES_DIR" ]]; then
  echo "Checking out dotfiles repository..."
  git clone --quiet --recursive "https://github.com/stevenspasbo/dotfiles.git" "$DOTFILES_DIR" > /dev/null
fi

mac_setup() {
  echo "Setting up envrionment for mac"
  # Check for homebrew
  if ! command -v brew > /dev/null; then
    echo "Installing homebrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi

  cd "$DOTFILES_DIR"
  brew bundle
}

linux_setup() {
  echo "Setting up linux environment"
}

case "$OSTYPE" in
  darwin*) mac_setup
           ;;
  linux*)  linux_setup
           ;;
  *)       echo "unknown: $OSTYPE"
           ;;
esac

# Install nvm
if ! command -v nvm > /dev/null; then
  echo "Installing nvm... "

  NVM_INSTALL_DIRECTORY="$HOME/.nvm"
  if [[ ! -d $NVM_INSTALL_DIRECTORY ]]; then
    git clone --quiet "https://github.com/creationix/nvm.git" "$NVM_INSTALL_DIRECTORY" > /dev/null
  fi

  cd "$NVM_INSTALL_DIRECTORY"
  LATEST_NVM_TAG=$(git describe --abbrev=0)
  git checkout "$LATEST_NVM_TAG" > /dev/null
  echo "nvm $LATEST_NVM_TAG installed."
  source "$NVM_INSTALL_DIRECTORY/nvm.sh"
  nvm install stable
fi

cd "$STARTING_DIRECTORY"
echo "Done"
