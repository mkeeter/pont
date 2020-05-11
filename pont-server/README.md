```
sudo apt update
sudo apt install build-essentials libssl-dev pkg-config
git clone https://github.com/mkeeter/pont.git
cd pont/pont-server
cargo run --release
```

# Using `nginx` + Let's Encrypt to get SSL
```
sudo apt install nginx
sudo apt-get install software-properties-common
sudo add-apt-repository universe
sudo add-apt-repository ppa:certbot/certbot
sudo apt-get update
sudo apt-get install certbot python3-certbot-nginx

sudo certbot --nginx
```

Deploy [`pont.conf`](/pont-server/pont.conf) to `/etc/nginx/sites-enabled/pont.conf`,
replacing `wss.mattkeeter.com` with your host name.
