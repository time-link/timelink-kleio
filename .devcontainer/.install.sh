echo "Post creation install of necessary tools"
echo
echo "Installing swi Prolog"
sudo apt-get update
sudo apt-get install --assume-yes software-properties-common
sudo apt-add-repository --yes ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install --assume-yes swi-prolog
echo
echo "Installing Postman cli tool Newman"
npm install -g newman