#!/usr/bin/env bash

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <file_path>"
    exit 1
fi

CMD1="perl perl/Main.pl"
CMD2="julia julia/main.jl"

GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo -e "******* Comparing commands (${GREEN}$CMD1${NC}) and (${GREEN}$CMD2${NC}) *******"
echo

for FILE_PATH in "$@"; do
    echo "Comparing output for file: $FILE_PATH"

    diff --side-by-side --suppress-common-lines <($CMD1 $FILE_PATH) <($CMD2 $FILE_PATH)

    if [ $?  -eq 0 ]; then
        echo "The outputs are identical for $FILE_PATH"
    else
        echo "The outputs differ for $FILE_PATH"
    fi

    echo
done
