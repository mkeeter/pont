#!/bin/bash
set -e -x

wasm-pack build --target web
mkdir -p deploy/pkg
curl -X POST -s --data-urlencode 'input@style.css' https://cssminifier.com/raw > deploy/style.css
curl -X POST -s --data-urlencode 'input@pkg/pont_client.js' https://javascript-minifier.com/raw > deploy/pkg/pont_client.js
rsync -R index.html pkg/pont_client_bg.wasm deploy

cat << EOF > deploy/files.txt
index.html
style.css
pkg/pont_client.js
pkg/pont_client_bg.wasm
EOF

rsync -aze ssh --update --files-from=deploy/files.txt deploy pont.mattkeeter.com:pont
rm -rf deploy
