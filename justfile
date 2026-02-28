[private]
@default: help

# show help message
@help:
    echo "Usage: just <recipe>"
    echo ""
    just --list

repl:
    cargo run -p repl

run FILE_PATH:
    cargo run -p runner {{FILE_PATH}}

test:
    cargo test

format:
    cargo fmt

lint:
    cargo clippy

build-wasm:
    wasm-pack build crates/runner --target web
