

################################################################
# NVM
################################################################
# Install node

[ ! -d $HOME/.nvm ] && git clone https://github.com/creationix/nvm.git ~/.nvm && sh ~/.nvm/install.sh
. $NVM_DIR/nvm.sh && nvm install stable
npm install -g grunt-cli

# gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
# \curl -sSL https://get.rvm.io | bash -s stable
