#!/usr/bin/env bash

set -uo pipefail

#set -x # uncomment this for debugging the script.

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
rm -rf {logs,asm,xml,bin,output}
mkdir {logs,asm,xml,bin,output}
popd

make clean 
make

COUNTER=0
for filename in $(find auto-tests -type f -name "*.$EXT" | sort); do 
    NAME="$(basename "$filename" .$EXT)"
    LOG_FILE="./auto-tests/logs/$NAME.log"
    echo "--------------------" >> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"
    echo "----START - XML!----" >> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"

    $TIL -g --target xml "$filename" -o ./auto-tests/xml/"$NAME".xml &> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"
    echo "----START - ASM!----" >> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"
    echo "--------------------" >> "$LOG_FILE"

    $TIL -g --target asm "$filename" -o ./auto-tests/asm/"$NAME".asm &>> "$LOG_FILE"
    yasm -felf32 ./auto-tests/asm/"$NAME".asm
#    mv ./auto-tests/"$NAME".asm ./auto-tests/asm/
    ld -m elf_i386 -o ./auto-tests/bin/"$NAME" "$NAME".o -lrts &> /dev/null
    rm "$NAME".o
    chmod +x ./auto-tests/bin/"$NAME"
    ./auto-tests/bin/"$NAME" | tr -d '\n' > ./auto-tests/output/"$NAME".out
    diff -iwub <(cat ./auto-tests/expected/"$NAME".out | tr -d '\n') <(cat ./auto-tests/output/"$NAME".out | tr -d '\n') > /dev/null

    
    if [ $? -eq 0 ]; then
       echo -e "${GREEN} Test $NAME passed${RESET}"
        ((COUNTER++)) || true
        rm ./auto-tests/output/"$NAME".out
    else
        echo -e "${RED} Test $NAME failed${RESET}"
    fi
    # break
done

echo "Passed $COUNTER tests out of $(find auto-tests -type f -name "*.$EXT" | wc -l)"
