#! /usr/bin/env bash

#-------------------------------------------------------------
# Author: Steven Spasbo
#
# This script is used to set up a new machine with my dotfiles
# and other tools.
#
# It is idempotent and can be run multiple times.
#-------------------------------------------------------------

#-------------------------------------------------------------
# Helper functions
#-------------------------------------------------------------
p() {
  printf "\n\033[1m\033[34m%s\033[0m\n\n" "${1}"
}

print_red() {
  printf "\n\033[1m\033[31m%s\033[0m\n\n" "${1}"
}

print_done() {
  printf "\033[1m\033[32m%s\033[0m\n" "Done"
}

#-------------------------------------------------------------
# Variables
#-------------------------------------------------------------
STARTING_DIRECTORY="$PWD"
DOTFILES_DIR="$HOME/dotfiles"
BACKUP_DIR="$DOTFILES_DIR/backups"
TIME=$(date +"%m_%d_%Y_-_%H_%M_%S")

#-------------------------------------------------------------
# Functions
#-------------------------------------------------------------
check_for_git() {
  echo -n "Checking for git... "
  if ! command -v git >/dev/null; then
    print_red "Install git first."
    exit 1
  fi

  print_done
}

check_out_dotfiles_repo() {
  echo -n "Checking out dotfiles repository..."
  if [[ ! -d "$DOTFILES_DIR" ]]; then
    if ! git clone --quiet --recursive "https://github.com/stevenspasbo/dotfiles.git" "$DOTFILES_DIR" >/dev/null; then
      print_red "Failed to clone dotfiles repository."
      exit 1
    fi
  else
    cd "$DOTFILES_DIR"
    if ! git fetch --quiet; then
      print_red "Failed to fetch dotfiles repository."
      exit 1
    fi
  fi

  print_done
}

link_dotfiles() {
  echo "Symlinking dotfiles... "
  if [[ "$PWD" != "$DOTFILES_DIR" ]]; then
    cd "$DOTFILES_DIR" || exit
  fi

  # Create backup directory if it doesn't exist
  if [[ ! -d "$BACKUP_DIR" ]]; then
    mkdir -p "$BACKUP_DIR"
  fi

  # Loop through all files in the dotfiles directory
  for file in dotfiles/*; do
    local home_file="$HOME/.$(basename "$file")"
    local base_dot_name=$(basename "$home_file")

    echo "Creating symlink for $base_dot_name"
    # If the file exists and is not a symlink, back it up
    if [[ -e "$home_file" && ! -L "$home_file" ]]; then
      local new_file_name="$BACKUP_DIR/$(basename "$file")$TIME"
      # If the backup file already exists, append a random number
      if [[ -e "$new_file_name" ]]; then
        new_file_name="$new_file_name.$RANDOM"
      fi
      if ! mv "$home_file" "$new_file_name"; then
        print_red "Failed to backup $home_file"
        continue
      fi
      echo "\t$home_file was moved to $new_file_name"
    fi

    # If the symlink already exists, check if it points to the correct file
    if [[ -L "$home_file" ]]; then
      if [[ "$(readlink "$home_file")" == "$DOTFILES_DIR/$file" ]]; then
        echo "\tSymlink for $base_dot_name already exists and is correct."
        continue
      else
        # If the symlink is incorrect, delete it
        rm "$home_file"
      fi
    fi

    # Create the symlink
    if ! ln -s "$DOTFILES_DIR/$file" "$home_file"; then
      print_red "Failed to create symlink for $base_dot_name"
    fi
  done

  print_done
}

create_exports_dot_local() {
  echo -n "Creating local exports file... "
  if [[ ! -f "$HOME/.exports.local" ]]; then
    if ! echo "#! /usr/bin/env $SHELL" >"$HOME/.exports.local"; then
      print_red "Failed to create .exports.local"
      return 1
    fi
  fi
  print_done
}

install_nvm() {
  # Install nvm
  echo -n "Installing nvm... "
  local NVM_INSTALL_DIRECTORY="$HOME/.nvm"
  if [[ ! -d $NVM_INSTALL_DIRECTORY ]]; then
    if ! git clone --quiet "https://github.com/creationix/nvm.git" "$NVM_INSTALL_DIRECTORY" >/dev/null; then
      print_red "Failed to clone nvm repository."
      return 1
    fi
  fi

  cd "$NVM_INSTALL_DIRECTORY" || exit
  if ! git fetch --quiet --tags origin; then
    print_red "Failed to fetch nvm tags."
    return 1
  fi

  local latest_tag
  latest_tag=$(git describe --abbrev=0 --tags --match "v[0-9]*" "$(git rev-list --tags --max-count=1)")
  if ! git checkout --quiet "$latest_tag"; then
    print_red "Failed to checkout nvm latest tag."
    return 1
  fi

  source "$NVM_INSTALL_DIRECTORY/nvm.sh"

  cd "$DOTFILES_DIR" || exit
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

  if ! find "$FONT_DIR" -iname "*powerline*.ttf" 2>/dev/null | grep -q "."; then
    if ! source "$DOTFILES_DIR/fonts/install.sh" >/dev/null; then
      print_red "Failed to install fonts."
      return 1
    fi
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
  if ! xcode-select --install 2>/dev/null; then
    #This will fail if xcode is already installed, which is fine.
    :
  fi
  print_done
}

install_homebrew() {
  if [[ ! "$OSTYPE" =~ "darwin" ]]; then
    return
  fi

  echo -n "Checking for homebrew... "
  if ! command -v brew >/dev/null; then
    echo "\nInstalling homebrew..."
    if ! /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"; then
      print_red "Failed to install homebrew."
      return 1
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
  check_for_git
  check_out_dotfiles_repo
  link_dotfiles
  create_exports_dot_local
  install_fonts
  install_nvm

  if [[ "$OSTYPE" =~ "darwin" ]]; then
    install_xcode
    install_homebrew
    install_homebrew_packages
  fi
}

main

cd "$STARTING_DIRECTORY"
echo "Set up complete"
