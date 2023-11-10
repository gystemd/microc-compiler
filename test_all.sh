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
        result="res.txt"
        outfile=${filepath%.mc}.out
        echo "file $outfile"
        echo "expected result"
        cat $outfile
        echo "----------------"
        echo "result"
        cat $result
        echo "----------------"
        if cmp "$res" "$outfile"; then
            echo "❌ test $filepath"
            echo "diff res.txt samples/$prefix$file.out"
        else
            echo "✅ test $outfile"
        fi
        # echo "to implement"
    fi
done

exit 0