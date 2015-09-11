brew tap homebrew/dupes > /dev/null
brew tap thoughtbot/formulae > /dev/null
brew tap homebrew/php > /dev/null

homebrew_formulas=(
    # Applications
    "ant"
    "archey"
    "caskroom/cask/brew-cask"
    "coreutils"
    "curl"
    "cvs"
    "emacs --cocoa"
    "faac"
    "findutils"
    "fontforge"
    "git"
    "thoughtbot/formulae/gitsh"
    "homebrew/dupes/grep"
    "htop-osx"
    "irssi"
    "maven"
    "nmap"
    "homebrew/fuse/sshfs"
    "tmux"
    "wget"

    # Shells
    "bash"
    "bash-completion"
    "zsh"

    # Languages
    "gcc"
    "leiningen"
    "plt-racket"
    "scala"
    "sbt"
    "homebrew/x11/mit-scheme"
    "python"
    "sbcl"

    # DBs
    "mysql"
    "mongodb"
    "postgresql"
    "readline"
    "sqlite"
)

cask_apps=(
    "vlc"
    "spotify"
    "google-chrome"
    "iterm2"
    "sublime-text"
    "osxfuse"
    "xquartz"
    "sourcetree"
)

if [ ! -e "/opt/homebrew-cask/Caskroom" ]; then
    brew cask
fi

for i in "${cask_apps[@]}"; do
    brew cask install --appdir="/Applications" ${cask_apps[@]}
done

# Install homebrew apps
for i in "${homebrew_formulas[@]}"; do
    brew install $i
    if [ $# == 1 ]; then
	echo "ERROR OMG"
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
