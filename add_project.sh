#!/bin/bash


# Check if at least one argument is supplied
if [ $# -eq 0 ]; then
    echo "No arguments supplied"
    exit 1
fi

# Check if directory exists
if [ -d "day_$1" ]; then
    # echo "Directory day_$1 already exists" in yellow
    echo -e "\e[33mDirectory day_$1 already exists\e[0m"
else
    # Create directory
    echo -e "\e[32mCreating directory day_$1\e[0m"
    mkdir "day_$1"
    mkdir "day_$1/first_star"
    mkdir "day_$1/second_star"
fi

# Check if second argument is supplied
if [ $# -eq 2 ]; then
    cd "day_$1" || exit
    case $2 in
        rs)
            # Rust specific setup
            echo copying rust projects
            cp -r ../templates/rust/ ./first_star
            cp -r ../templates/rust/ ./second_star
            cd ../ || exit
            ;;
        hs)
            # Haskell specific setup
            echo copying haskell projects
            cp -r ../templates/haskell/ ./first_star
            cp -r ../templates/haskell/ ./second_star
            cd ../ || exit
            ;;
        zig)
            # Zig specific setup
            echo copying zig projects
            cp -r ../templates/zig/ ./first_star
            cp -r ../templates/zig/ ./second_star
            cd ../ || exit
            ;;
        node)
            # Node.js specific setup
            echo copying node projects
            cp -r ../templates/node/ ./first_star
            cd first_star/node || exit
            pnpm install
            cd ../../ || exit
            cp -r ../templates/node/ ./second_star
            cd second_star/node || exit
            pnpm install
            cd ../../ || exit
            ;;
        bun)
            # Bun specific setup
            echo copying bun projects
            cp -r ../templates/bun/ ./first_star
            cd first_star/bun || exit
            bun install
            cd ../../ || exit
            cp -r ../templates/bun/ ./second_star
            cd second_star/bun || exit
            bun install
            cd ../../ || exit
            ;;
        input)
            # Input specific setup
            echo downloading input
            # shellcheck disable=SC1091
            source ../.venv/bin/activate
            aocd "$1" 2022 > input.txt
            ;;
        *)
            echo "Invalid language supplied"
            exit 1
            ;;
    esac
fi