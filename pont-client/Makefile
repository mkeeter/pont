all:
	@echo "Must specify target (e.g. 'make deploy')"

.PHONY: pack deploy
pack:
	wasm-pack build --target web

deploy/index.html: index.html
	cp $< $@
deploy/style.css: style.css
	curl -X POST -s --data-urlencode 'input@$<' https://cssminifier.com/raw > $@
deploy/pkg/pont_client.js: pkg/pont_client.js
	mkdir -p deploy/pkg
	curl -X POST -s --data-urlencode 'input@$<' https://javascript-minifier.com/raw > $@
deploy/pkg/pont_client_bg.wasm: pkg/pont_client_bg.wasm
	mkdir -p deploy/pkg
	cp $< $@

pkg/pont_client_bg.wasm: pack
pkg/pont_client.js: pack

DEPS=$(shell cat deploy/files.txt)
deploy: $(addprefix deploy/, $(DEPS))
	rsync -aze ssh --update --files-from=deploy/files.txt deploy pont.mattkeeter.com:pont
