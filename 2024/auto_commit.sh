#!/bin/bash

# Check if the correct number of arguments are provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <day>"
    exit 1
fi

day=$1

# Define the path to the file based on the language
cd "day_$day" || exit
git stage first_star/Cargo.toml
git stage first_star/src/main.rs
git stage second_star/Cargo.toml
git stage second_star/src/main.rs
git commit -m "add rust solution for day $day"
