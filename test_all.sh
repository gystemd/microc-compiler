#!/bin/bash
#!/bin/bash

#read the folder in input
folder=$1
clang="clang -w"
microcc="bin/microcc.exe"
for file in $folder/*fail*.mc; do
    name=${file%%.*}
    dune exec -- $microcc $name.mc -o $name.bc > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Test $name ✅"
    else
        echo "Test $name ❌"
    fi
done

for file in $folder/test*.mc; do
    name=${file%%.*}
    dune exec -- $microcc $name.mc -o $name.bc
    if [ $? -ne 0 ]; then
        break
    fi
    $clang $name.bc bin/rt-support.c -o $name.elf
    diff <($name.elf) $name.out
    if [ $? -ne 0 ]; then
        rm -f $name.elf
        echo "Test $name ❌"
    fi
    echo "Test $name ✅"
    rm -f $name.elf

done

rm -f $folder/*.bc
rm -f $folder/*.elf