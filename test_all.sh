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
           echo "❌ test $filepath"
        else
           echo "✅ test $filepath"
        fi
    else
        dune exec bin/microcc.exe -- $filepath
        clang a.bc bin/rt-support.c
        ./a.out > res.txt
        if diff res.txt samples/$prefix$file.out; then
            echo "❌ test $filepath"
            echo "diff res.txt samples/$prefix$file.out"
        else
            echo "✅ test $filepath"
        fi
        # echo "to implement"
    fi
done