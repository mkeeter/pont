#!/bin/bash
set -e -x

wasm-pack build --target web
mkdir -p deploy/pkg
curl -X POST -s --data-urlencode 'input@style.css' https://cssminifier.com/raw > deploy/style.css
curl -X POST -s --data-urlencode 'input@pkg/pont_client.js' https://javascript-minifier.com/raw > deploy/pkg/pont_client.js
curl -X POST -s --data-urlencode 'input@index.html' https://html-minifier.com/raw > deploy/index.html
cp pkg/pont_client_bg.wasm deploy/pkg

cat << EOF > deploy/files.txt
.htaccess
index.html
style.css
pkg/pont_client.js
pkg/pont_client_bg.wasm
EOF

rsync -aze ssh --update --files-from=deploy/files.txt deploy pont.mattkeeter.com:pont
rm -rf deploy
