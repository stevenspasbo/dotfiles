brew tap homebrew/dupes > /dev/null

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
    brew install $i
done

if [ ! -e "/opt/homebrew-cask/Caskroom" ]; then
    brew cask
fi
for i in in "${cask_apps[@]}"
do
    brew cask install --appdir="/Applications" ${cask_apps[@]}
done


# Install fonts
brew tap caskroom/fonts
fonts=(
  font-m-plus
  font-clear-sans
  font-roboto
)
echo "installing fonts..."
brew cask install ${fonts[@]}

