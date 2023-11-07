#!/bin/bash
prefix="fail"
# iterate all files in the samples directory
for filepath in samples/*; do
    file=$(basename "$filepath")

    if [[ "$file" == *"out" ]]; then
        continue
    fi

    if [[ "$file" == "fail"* ]]; then
        if dune exec bin/microcc.exe -- $filepath > /dev/null; then
           echo "something wrong in $filepath"
        else
           echo "test $filepath passed"
        fi
    else
        # dune exec bin/microcc.exe -- $filepath
        # clang a.bc bin/rt-support.c
        # ./a.out > res.txt
        echo "to implement"
    fi
done