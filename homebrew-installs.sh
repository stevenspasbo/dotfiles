brew tap homebrew/dupes > /dev/null
brew tap thoughtbot/formulae > /dev/null
brew tap homebrew/php

homebrew_formulas=(
    "coreutils"
    "wget"
    "findutils"
    "homebrew/dupes/grep"
    "sshfs"
    "node"
    "npm"
    "htop"
    "tmux"
    "git"
    "caskroom/cask/brew-cask"
    "gitsh"
    "bash"
    "zsh"
    "ant"
    "plt-racket"
    "php55"
    "scala"
    "emacs"
    "mit-scheme"
)

cask_apps=(
    "vlc"
    "spotify"
    "google-chrome"
    "iterm2"
    "sublime-text2"
)

# Install homebrew apps
for i in "${homebrew_formulas[@]}"
do
  echo "Install $@? (y/n): "
  read ans
  if (( (ans == "y") || ans == "yes")); then
    brew install $i
  else
    echo "Skipping $@"
  fi
done

if [ ! -e "/opt/homebrew-cask/Caskroom" ]; then
    brew cask
fi
for i in in "${cask_apps[@]}"
do
  echo "Install $@? (y/n): "
  read ans
  if (( (ans == "y") || ans == "yes")); then
    brew cask install --appdir="/Applications" ${cask_apps[@]}
  else
    echo "Skipping $@"
  fi
done


# Install fonts
brew tap caskroom/fonts
fonts=(
  font-m-plus
  font-clear-sans
  font-roboto
)
# echo "installing fonts..."
# brew cask install ${fonts[@]}

