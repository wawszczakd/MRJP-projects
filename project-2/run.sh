#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <folder>"
    exit 1
fi

folder="$1"
if [ ! -d "$folder" ]; then
    echo "Error: Folder '$folder' does not exist."
    exit 1
fi

make

for file in "$folder"/*.lat; do
    if [ ! -e "$file" ]; then
        echo "No .lat files found in the folder."
        exit 0
    fi
    
    echo -ne "\nProcessing $file...\n"
    ./latte "$file"
done
