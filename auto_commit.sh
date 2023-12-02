#!/bin/bash

# Check if the correct number of arguments are provided
if [ $# -ne 2 ]; then
    echo "Usage: $0 <day> <language>"
    exit 1
fi

day=$1
language=$2

# Define the path to the file based on the language
case $language in
    rs)
        cd "day_$day" || exit
        git stage first_star/rust/Cargo.toml
        git stage first_star/rust/src/main.rs
        git stage second_star/rust/Cargo.toml
        git stage second_star/rust/src/main.rs
        git commit -m "add rust solution for day $day"
        ;;
    hs)
        cd "day_$day" || exit
        git stage first_star/haskell/package.yaml
        git stage first_star/haskell/haskell.cabal
        git stage first_star/haskell/stack.yaml
        git stage first_star/haskell/stack.yaml.lock
        git stage first_star/haskell/app/Main.hs
        git stage second_star/haskell/package.yaml
        git stage second_star/haskell/haskell.cabal
        git stage second_star/haskell/stack.yaml
        git stage second_star/haskell/stack.yaml.lock
        git stage second_star/haskell/app/Main.hs
        git commit -m "add haskell solution for day $day"
        ;;
    zig)
        cd "day_$day" || exit
        git stage first_star/zig/src/main.zig
        git stage first_star/zig/build.zig
        git stage second_star/zig/src/main.zig
        git stage second_star/zig/build.zig
        git commit -m "add zig solution for day $day"
        ;;
    node)
        echo finish this one
        exit 1
        ;;
    bun)
        cd "day_$day" || exit
        git stage first_star/bun/.gitignore
        git stage first_star/bun/bun.lockb
        git stage first_star/bun/index.ts
        git stage first_star/bun/package.json
        git stage first_star/bun/tsconfig.json
        git stage second_star/bun/.gitignore
        git stage second_star/bun/bun.lockb
        git stage second_star/bun/index.ts
        git stage second_star/bun/package.json
        git stage second_star/bun/tsconfig.json
        git commit -m "add bun solution for day $day"
        ;;
    input)
        cd "day_$day" || exit
        git stage input.txt
        git commit -m "add input for day $day"
        ;;
    *)
        echo "Invalid language supplied"
        exit 1
        ;;
esac