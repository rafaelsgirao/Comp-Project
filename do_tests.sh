#!/usr/bin/env bash

set -uo pipefail

# set -x # uncomment this for debugging the script.

# How to use:
#   place this script at the root of your project
#   copy DM's 'auto-tests' folder to the root of you project
#   Change the $EXT variable below to match your language's file extension
#   e.g, .til, .mml, .spl, .og, ...
#   Run this script with: bash <script's name.sh>

EXT='til'
RED='\033[0;31m'
#YELLOW='\033[0;33m'
GREEN='\033[0;32m'
#BLUE='\033[0;34m'
#BOLD='\033[1m'
RESET='\033[0m'

TIL="$(pwd)/til"

pushd ./auto-tests/
rm -rf {logs,asm,bin,output}
mkdir {logs,asm,bin,output}
popd


make clean 
make


COUNTER=0
for filename in $(find auto-tests -type f -name "*.$EXT" | sort); do 
    NAME="$(basename "$filename" .$EXT)"
    $TIL -g --target asm "$filename" -o ./auto-tests/asm/"$NAME".asm &> ./auto-tests/logs/"$NAME".log
    yasm -felf32 ./auto-tests/asm/"$NAME".asm
#    mv ./auto-tests/"$NAME".asm ./auto-tests/asm/
    ld -m elf_i386 -o ./auto-tests/bin/"$NAME" "$NAME".o -lrts &> /dev/null
    rm "$NAME".o
    chmod +x ./auto-tests/bin/"$NAME"
    ./auto-tests/bin/"$NAME" > ./auto-tests/output/"$NAME".out
    diff -iwc  ./auto-tests/expected/"$NAME".out ./auto-tests/output/"$NAME".out > /dev/null
    diff -iwub ./auto-tests/expected/"$NAME".out ./auto-tests/output/"$NAME".out > /dev/null

    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN} Test $NAME passed${RESET}"
        ((COUNTER++))
        rm ./auto-tests/output/"$NAME".out
    else
        echo -e "${RED} Test $NAME failed${RESET}"
    fi
#    break
done

echo "Passed $COUNTER tests out of $(find auto-tests -type f -name "*.$EXT" | wc -l)"