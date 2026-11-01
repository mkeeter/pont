PKG_DIR    := "pont-client/pkg" # directory in which we assemble files
PKG_WA     := PKG_DIR / "pont_client.opt.wasm"
PKG_JS     := PKG_DIR / "pont_client.js"
PKG_CSS    := STATIC_DIR / "style.css"

DIST_DIR   := "pont-client/dist"
STATIC_DIR := "pont-client/static"
CUT8       := ".{56}$" # regex to strip the trailing 56 characters

cargo-web := "RUSTFLAGS='--cfg getrandom_backend=\"wasm_js\"' cargo"
dist:
    # Build the client code
    rustup target add wasm32-unknown-unknown --toolchain stable
    {{cargo-web}} build --release -ppont-client --target wasm32-unknown-unknown

    # Move it into the PKG dir and generate bindings
    rm -rf {{PKG_DIR}}/*
    wasm-bindgen target/wasm32-unknown-unknown/release/pont_client.wasm --out-dir {{PKG_DIR}} --target web
    wasm-opt -O {{PKG_DIR}}/pont_client_bg.wasm -o {{PKG_WA}}

    # Assemble everything in the DIST dir
    mkdir -p {{DIST_DIR}}
    rm    -rf {{DIST_DIR}}/*
    cp {{PKG_WA}} '{{DIST_DIR}}/pont_client.{{replace_regex(sha256_file(PKG_WA), CUT8, "")}}.wasm'
    cp {{PKG_JS}} '{{DIST_DIR}}/pont_client.{{replace_regex(sha256_file(PKG_JS), CUT8, "")}}.js'
    cat {{STATIC_DIR}}/index.html \
        | sed s/JSHASH/{{replace_regex(sha256_file(PKG_JS), CUT8, "")}}/g \
        | sed s/WAHASH/{{replace_regex(sha256_file(PKG_WA), CUT8, "")}}/g \
        | sed s/CSSHASH/{{replace_regex(sha256_file(PKG_CSS), CUT8, "")}}/g \
        > {{DIST_DIR}}/index.html
    cp pont-client/static/htaccess {{DIST_DIR}}/.htaccess
    cp pont-client/static/style.css {{DIST_DIR}}/style.{{replace_regex(sha256_file(PKG_CSS), CUT8, "")}}.css
    cat {{STATIC_DIR}}/rules.html  \
        | sed 's/style\.css/style.{{replace_regex(sha256_file(PKG_CSS), CUT8, "")}}.css/g' \
        > {{DIST_DIR}}/rules.html
    cp {{STATIC_DIR}}/favicon.ico {{DIST_DIR}}/favicon.ico

serve:
    just dist
    python3 -m http.server -d {{DIST_DIR}}
