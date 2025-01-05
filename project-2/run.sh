#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

script_dir=$(dirname "$(realpath "$0")")

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <folder>"
    exit 1
fi

folder="$1"
if [ ! -d "$folder" ]; then
    echo "Error: Folder '$folder' does not exist."
    exit 1
fi

make -C "$script_dir"

for file in "$folder"/*.lat; do
    if [ ! -e "$file" ]; then
        echo "No .lat files found in the folder."
        exit 0
    fi
    
    echo -ne "Processing: $file...\t"
    "$script_dir/latc_llvm" "$file" 2> /dev/null
    
    bc_file="${file%.lat}.bc"
    expected_output="${file%.lat}.output"
    input_file="${file%.lat}.input"
    
    if [ -e "$bc_file" ]; then
        if [ -e "$input_file" ]; then
            lli "$bc_file" < "$input_file" 1> output.log 2> /dev/null
        else
            lli "$bc_file" 1> output.log 2> /dev/null
        fi
        
        if [ -e "$expected_output" ]; then
            diff output.log "$expected_output" > /dev/null
            if [ $? -eq 0 ]; then
                echo -e "${GREEN}OK${NC}"
            else
                echo -e "${RED}Wrong${NC}"
            fi
        else
            echo -e "${RED}Wrong${NC}"
        fi
    else
        echo -e "${RED}Wrong${NC}"
    fi
done
