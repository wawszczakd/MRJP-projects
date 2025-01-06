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

compile_files() {
    make -C "$script_dir"
    
    for file in "$folder"/*.lat; do
        if [ ! -e "$file" ]; then
            echo "No .lat files found in the folder."
            exit 0
        fi
        
        echo -ne "Processing $file... \t"
        "$script_dir/latc_llvm" "$file"
    done
}

run_files() {
    for file in "$folder"/*.lat; do
        if [ ! -e "$file" ]; then
            echo "No .lat files found in the folder."
            exit 0
        fi
        
        echo -ne "Running $file... \t"
        
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
                    echo -e "${RED}Wrong${NC} Ouputs differ"
                fi
            else
                echo -e "${RED}Wrong${NC} File $expected_output does not exist"
            fi
        else
            echo -e "${RED}Wrong${NC} File $bc_file does not exist"
        fi
        
        rm -f output.log
    done
}

echo "What would you like to do?"
echo "1) Compile files"
echo "2) Run files"
read -p "Enter your choice (1 or 2): " choice

case $choice in
    1)
        compile_files "$folder"
        ;;
    2)
        run_files "$folder"
        ;;
    *)
        echo "Invalid choice. Please enter 1 or 2."
        ;;
esac
