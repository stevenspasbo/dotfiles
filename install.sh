#! /usr/bin/env bash

p () {
  printf "\n\033[1m\033[34m%s\033[0m\n\n" "${1}"
}

print_red() {
  printf "\n\033[1m\033[31m%s\033[0m\n\n" "${1}"
}

print_done () {
  printf "\033[1m\033[32m%s\033[0m\n" "Done"
}

STARTING_DIRECTORY="$PWD"
cd "$HOME"

check_for_git() {
  echo -n "Checking for git... "
  if ! command -v git > /dev/null; then
    print_red "Install git first."
    exit -1
  fi

  print_done
}

check_out_dotfiles_repo() {
  echo -n "Checking out dotfiles repository..."
  DOTFILES_DIR="$HOME/dotfiles"
  if [[ ! -d "$DOTFILES_DIR" ]]; then
    git clone --quiet --recursive "https://github.com/stevenspasbo/dotfiles.git" "$DOTFILES_DIR" > /dev/null
  else
    cd "$DOTFILES_DIR"
    git fetch --quiet
  fi

  print_done
}

link_dotfiles() {
  echo -n "Symlinking dotfiles... "
  if [[ "$PWD" != "$DOTFILES_DIR" ]]; then
    cd "$DOTFILES_DIR"
  fi
  # TODO Remove the dependency on rake, script this.
  if ! command -v rake > /dev/null; then
    echo "\nRake not found, exiting"
    exit -1
  fi

  rake install_dotfiles
  print_done
}

create_exports_dot_local() {
  echo -n "Creating local exports file... "
  if [[ ! -f "$HOME/.exports.local" ]]; then
    echo "#! /usr/bin/env \$SHELL\n\n" > "$HOME/.exports.local"
  fi
  print_done
}

install_nvm() {
  # Install nvm
  echo -n "Installing nvm... "
  if ! command -v nvm > /dev/null; then
    local NVM_INSTALL_DIRECTORY="$HOME/.nvm"
    if [[ ! -d $NVM_INSTALL_DIRECTORY ]]; then
      git clone --quiet "https://github.com/creationix/nvm.git" "$NVM_INSTALL_DIRECTORY" > /dev/null
    fi
  fi

  cd "$NVM_INSTALL_DIRECTORY"
  git fetch --quiet --tags origin
  git checkout --quiet `git describe --abbrev=0 --tags --match "v[0-9]*" $(git rev-list --tags --max-count=1)`

  source "$NVM_INSTALL_DIRECTORY/nvm.sh"

  cd "$DOTFILES_DIR"
  print_done
}

install_pyenv() {
  local PYENV_INSTALL_DIR="$HOME/.pyenv"
  if [[ ! -d "$PYENV_INSTALL_DIR" ]]; then
    git clone https://github.com/pyenv/pyenv.git "$PYENV_INSTALL_DIR" > /dev/null
    # fgrep "export PYENV_ROOT=\"\$HOME/.pyenv\"'" .exports.local
    # fgrep "export PATH=\"\$PYENV_ROOT/bin:$PATH\"'" .exports.local
  fi
  print_done
}

install_fonts() {
  echo -n "Checking for powerline fonts... "
  local FONT_DIR
  if [[ "$OSTYPE" =~ "darwin" ]]; then
    FONT_DIR="$HOME/Library/Fonts"
  else
    FONT_DIR="$HOME/.local/share/fonts"
  fi

  if ! find "$FONT_DIR" -iname "*powerline*.ttf" 2> /dev/null | grep -q "."; then
    source "$DOTFILES_DIR/fonts/install.sh" > /dev/null
  fi

  print_done
}

##################################################
# Mac stuff
##################################################

install_xcode() {
  if [[ ! "$OSTYPE" =~ "darwin" ]]; then
    return
  fi

  echo -n "Checking for xcode... "
  xcode-select --install 2> /dev/null
  print_done
}

install_homebrew() {
  if [[ ! "$OSTYPE" =~ "darwin" ]]; then
    return
  fi

  echo -n "Checking for homebrew... "
  if ! command -v brew > /dev/null; then
    echo "\nInstalling homebrew..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi
  print_done
}

install_homebrew_packages() {
  if [[ ! "$OSTYPE" =~ "darwin" ]]; then
    return
  fi

  echo -n "Installing packages from homebrew... "
  # cd "$DOTFILES_DIR"
  # brew bundle
  print_done
}

set_homebrew_zsh_as_default_shell() {
  if [[ ! "$OSTYPE" =~ "darwin" ]]; then
    return
  fi

  if ! command -v brew > /dev/null; then
    return
  fi

  local BREW_PREFIX=$(brew --prefix)
  local HOMEBREW_ZSH="${BREW_PREFIX}/bin/zsh"

  if ! fgrep -q "$HOMEBREW_ZSH" /etc/shells; then
    echo -n "Adding homebrew zsh to /etc/shells... "
    echo "$HOMEBREW_ZSH" | sudo -t /etc/shells
    print_done
  fi

  if [[ ! "$SHELL" == "$HOMEBREW_ZSH" ]]; then
    echo -n "Setting user's shell to be newer zsh... "
    chsh -s "$HOMEBREW_ZSH"
    print_done
  fi
}

main() {
  check_out_dotfiles_repo
  link_dotfiles
  create_exports_dot_local
  install_xcode
  install_fonts
  install_homebrew
  install_homebrew_packages
  set_homebrew_zsh_as_default_shell
  install_nvm
}

main

cd "$STARTING_DIRECTORY"
echo "Set up complete"
