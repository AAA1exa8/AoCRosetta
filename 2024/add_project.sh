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
    exit 1
else
    # Attempt to create a directory and exit if it fails
    if mkdir "day_$1"; then
        echo -e "\e[32mCreating directory day_$1\e[0m"
    else
        echo -e "\e[31mFailed to create directory day_$1\e[0m"
        exit 1
    fi
fi

# Input specific setup
cd "day_$1" || exit
echo downloading input

# Activate the Python virtual environment and download input
# shellcheck disable=SC1091
source ../../.venv/bin/activate
aocd "$1" 2024 > input.txt

echo setting up rust projects
cargo new first_star --bin
cargo new second_star --bin
cp ../main.rs first_star/src/main.rs
cp ../main.rs second_star/src/main.rs

