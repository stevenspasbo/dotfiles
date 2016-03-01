brew tap homebrew/dupes 2>&1 /dev/null
brew tap thoughtbot/formulae 2>&1 /dev/null
brew tap homebrew/php 2>&1 /dev/null
brew tap caskroom/fonts 2>&1 /dev/null
brew tap dart-lang/dart 2>&1 /dev/null

homebrew_formulas=(
    # Applications
    "ant"
    "archey"
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
    "readline"
    "cloc"
    "caskroom/cask/brew-cask"

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
    "dart --with-content-shell --with-dartium"

    # DBs
    "mysql"
    "mongodb"
    "postgresql"
    "sqlite"
)

# Install homebrew apps
for i in "${homebrew_formulas[@]}"; do
    brew install $i
done

cask_apps=(
    "vlc"
    "spotify"
    "google-chrome"
    "iterm2"
    "sublime-text"
    "osxfuse"
    "xquartz"
    "sourcetree"
    "racket"
    "java"
    "dropbox"
    "evernote"
    "caskroom/homebrew-versions/java6"
    "phpstorm"
    "textmate"
    "spotifree"
    "ynab"
)

if [ ! -e "/opt/homebrew-cask/Caskroom" ]; then
    brew cask
fi

for i in "${cask_apps[@]}"; do
    brew cask install $i
done

# Install fonts
# fonts=(
#     font-m-plus
#     font-clear-sans
#     font-roboto
# )
# echo "installing fonts..."
# brew cask install ${fonts[@]}
