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
fi

# Input specific setup
# Check if second argument is supplied
cd "day_$1" || exit
echo downloading input
# shellcheck disable=SC1091
source ../../.venv/bin/activate
aocd "$1" 2024 > input.txt
echo setting up rust projects
cargo new first_star --bin
cargo new second_star --bin
cp ../main.rs first_star/src/main.rs
cp ../main.rs second_star/src/main.rs
