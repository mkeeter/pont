# About
`pont` is an online game based on
[Qwirkle (by Mindware Games)](https://en.wikipedia.org/wiki/Qwirkle)

Notably, both the client and server are written in Rust;
the only Javascript is a shim to load the WebAssembly module.

# Hosting
It's easiest to run the whole application on a single VM,
using [NGINX](https://www.nginx.com/) to both serve static content
and to act as a secure proxy for websocket communication.
The latter lets our server app not worry about SSL itself.

## Initial setup
```
sudo apt update
sudo apt install build-essentials libssl-dev pkg-config
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

## Installing NGINX and setting up Let's Encrypt
```
sudo apt install nginx
sudo apt-get install software-properties-common
sudo add-apt-repository universe
sudo add-apt-repository ppa:certbot/certbot
sudo apt-get update
sudo apt-get install certbot python3-certbot-nginx

sudo certbot --nginx
```
(read and follow `certbot`'s instructions)

## Turn on a firewall to improve security
```
sudo ufw allow ssh
sudo ufw allow http
sudo ufw allow https
sudo ufw allow 8081
sudo ufw enable
```

## Building the client webassembly file
```
git clone https://github.com/mkeeter/pont.git
cd pont/pont-client
wasm-pack build --target web
```

## Deploy the nginx config
```
sudo cp pont.conf /etc/nginx/sites-enabled/pont.conf
sudo rm /etc/nginx/sites-enabled/default
sudo nginx -s reload
```
This won't work out of the box, because the configuration assumes the url is
`pont.mattkeeter.com`, which won't be true for you; edit it accordingly.

## Running the server
```
cd pont/pont-server
cargo run --release
```
(leave this in a `screen` session for easy persistence!)
