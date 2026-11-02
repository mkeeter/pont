# About
### [Project homepage](https://mattkeeter.com/projects/pont)

`pont` is an online game based on
[Qwirkle (by Mindware Games)](https://en.wikipedia.org/wiki/Qwirkle)

![Screenshot](https://mattkeeter.com/projects/pont/screenshot.png)

Notably, both the client and server are written in Rust;
the only Javascript is a shim to load the WebAssembly module.

# Hosting
It's easiest to run the whole application on a single VM,
using [NGINX](https://www.nginx.com/) to both serve static content
and to act as a secure proxy for websocket communication.
The latter means we don't need SSL support in the game server itself.

The system looks something like this:

![Screenshot](https://mattkeeter.com/projects/pont/diagram.svg)

I'm hosting a copy of the game at
[https://pont.mattkeeter.com](https://pont.mattkeeter.com),
using a $5/month droplet from [Digital Ocean](https://www.digitalocean.com/)
and [Dreamhost](https://www.dreamhost.com/) for domain registration.

## Initial setup
The droplet boots up with only the root user enabled.  Create a user account
under which to run the game, e.g.
```console
useradd -m -g users pont
sudo usermod -aG sudo pont
sudo passwd pont # then pick a password
```

Switch to that user account, then install some packages:
```
su pont
sudo apt update
sudo apt install build-essential just libssl-dev pkg-config binaryen
```

## Installing NGINX and setting up Let's Encrypt
```
sudo apt install nginx
sudo snap install --classic certbot
sudo certbot certonly --nginx -d pont.mattkeeter.com
```
(read and follow `certbot`'s instructions)

## Turn on a firewall to improve security
```
sudo ufw allow ssh
sudo ufw allow 'Nginx Full'
sudo ufw allow 8081
sudo ufw enable
```

## Building the client WebAssembly file
We'll use `cargo binstall` to acquire `wasm-bindgen`, because building it from
scratch fills up the servers RAM and starts swapping.
```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash
cargo binstall wasm-bindgen-cli
git clone https://github.com/mkeeter/pont.git
cd pont
sudo apt install just
just dist
sudo mkdir /var/www/pont
sudo cp -r pont-client/dist/* /var/www/pont
```

## Deploy the nginx config
```
sudo cp pont.conf /etc/nginx/sites-available/pont.conf
sudo ln -s /etc/nginx/sites-available/pont.conf /etc/nginx/sites-enabled/
sudo rm /etc/nginx/sites-enabled/default
sudo nginx -t && sudo systemctl reload nginx
```
This won't work out of the box, because the configuration assumes the url is
`pont.mattkeeter.com`, which won't be true for you; edit it accordingly.

## Running the server
```
cargo run --release -ppont-server
```
(leave this in a `screen` session for easy persistence!)

# License
© 2020-2026 [Matthew Keeter](https://mattkeeter.com)

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
