#!/usr/bin/env bash

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <file_path>"
    exit 1
fi

CMD1="go run go/main.go go/hand.go go/card.go go/hand_sorter.go go/hand_identifier.go"
CMD2="julia julia/main.jl"


echo "******* Comparing commands ($CMD1) and ($CMD2) *******"

for FILE_PATH in "$@"; do
    echo "Comparing output for file: $FILE_PATH"

    diff <($CMD1 $FILE_PATH) <($CMD2 $FILE_PATH)

    if [ $?  -eq 0 ]; then
        echo "The outputs are identical for $FILE_PATH"
    else
        echo "The outputs differ for $FILE_PATH"
    fi

    echo
done
