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
DOTFILES_DIR="$HOME/dotfiles"


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

  if [[ ! -d "$DOTFILES_DIR" ]]; then
    git clone --quiet --recursive "https://github.com/stevenspasbo/dotfiles.git" "$DOTFILES_DIR" > /dev/null
  else
    cd "$DOTFILES_DIR"
    git fetch --quiet
  fi

  print_done
}

link_dotfiles() {
  echo "Symlinking dotfiles... "
  for FILE in $(find "$DOTFILES_DIR/dotfiles" -maxdepth 1 -mindepth 1); do
    local DOTFILE_DEST="$HOME/.$(basename $FILE)"
    if [[ -f "$DOTFILE_DEST" ]]; then
      echo "$DOTFILE_DEST already exists, skipping."
    else
      ln -s "$FILE" "$HOME/.$(basename $FILE)"
    fi
  done

  print_done
}

create_exports_dot_local() {
  echo -n "Creating local exports file... "
  if [[ ! -f "$HOME/.exports.local" ]]; then
    echo "#! /usr/bin/env \$SHELL\n" > "$HOME/.exports.local"
  fi

  print_done
}

install_pyenv() {
  echo -n "Installing pyenv... "
  local PYENV_INSTALL_DIR="$HOME/.pyenv"
  if [[ ! -d "$PYENV_INSTALL_DIR" ]]; then
    git clone "https://github.com/pyenv/pyenv.git" "$PYENV_INSTALL_DIR" > /dev/null
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
  if command -v xcode-select > /dev/null; then
    xcode-select --install 2> /dev/null
  fi

  print_done
}

install_homebrew() {
  if [[ ! "$OSTYPE" =~ "darwin" ]]; then
    return
  fi

  if [[ ! -f "$HOME/.exports.local" ]]; then
    print_red "Local exports file not found, exiting."
  fi

  echo -n "Checking for homebrew... "
  if ! command -v brew > /dev/null; then
    # Ensure it wasn't installed but set up incorrectly.
    if [[ -d /opt/homebrew ]]; then
      echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> "$HOME/.exports.local"
    else
      echo "\nInstalling homebrew..."
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  
    fi
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

main() {
  install_xcode
  check_out_dotfiles_repo
  link_dotfiles
  create_exports_dot_local
  install_fonts
  # install_homebrew
  # install_homebrew_packages
}

main

cd "$STARTING_DIRECTORY"
echo "Set up complete"
