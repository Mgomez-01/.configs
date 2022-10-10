# the basics that I use everywhere
sudo apt-get update -y && sudo apt-get upgrade -y
sudo apt-get install -y mc \
git \
rust-all \
ssh \
emacs \
emacs-common 


# vivaldi browser install stuff
sudo apt install wget gnupg2 software-properties-common -y
wget -qO- https://repo.vivaldi.com/archive/linux_signing_key.pub | sudo apt-key add -

echo 'deb https://repo.vivaldi.com/archive/deb/ stable main' | sudo tee /etc/apt/sources.list.d/vivaldi.list
sudo apt update
sudo apt install -y vivaldi-stable
sudo apt install htop -y
sudo apt install -y tldr lolcat figlet


# add the emacs27 stuff here


# Rofi launcher
sudo apt-get install -y rofi


# starship installer
curl -sS https://starship.rs/install.sh | sh
