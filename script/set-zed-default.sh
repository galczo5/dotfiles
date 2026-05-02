#!/bin/bash

# Set Zed as the default app for common text and source code file types.
# Requires: brew install duti

set -e

if ! command -v duti >/dev/null 2>&1; then
    echo "duti not found. Install with: brew install duti"
    exit 1
fi

ZED_BUNDLE_ID="dev.zed.Zed"

UTIS=(
    public.plain-text
    public.text
    public.utf8-plain-text
    public.utf16-plain-text
    public.source-code
    public.script
    public.shell-script
    public.python-script
    public.ruby-script
    public.perl-script
    public.php-script
    public.json
    public.xml
    public.yaml
    public.html
    public.css
    com.netscape.javascript-source
    public.c-source
    public.c-plus-plus-source
    public.c-header
    public.objective-c-source
    public.swift-source
    net.daringfireball.markdown
)

EXTS=(
    txt md markdown rst log csv tsv
    js mjs cjs jsx ts tsx
    py pyi pyx rb go rs
    c h cc cpp hpp cxx hxx
    java kt kts scala clj cljs
    swift m mm
    php
    sh bash zsh fish ksh
    lua vim el
    json json5 jsonc xml yaml yml toml ini conf cfg env
    html htm xhtml css scss sass less styl
    sql graphql gql
    Dockerfile dockerfile makefile mk
    gitignore gitattributes gitconfig
    editorconfig prettierrc eslintrc
    plist
)

for uti in "${UTIS[@]}"; do
    duti -s "$ZED_BUNDLE_ID" "$uti" all 2>/dev/null && echo "[OK] $uti" || echo "[SKIP] $uti"
done

for ext in "${EXTS[@]}"; do
    duti -s "$ZED_BUNDLE_ID" ".$ext" all 2>/dev/null && echo "[OK] .$ext" || echo "[SKIP] .$ext"
done
