#!/bin/bash
#!/bin/bash

clang="clang -w"
microcc="bin/microcc.exe"
for file in samples/*fail*.mc; do
    name=${file%%.*}
    dune exec -- $microcc $name.mc -o $name.bc > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Test $name ✅"
    else
        echo "Test $name ❌"
    fi
done

for file in samples/test*.mc; do
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

rm -f samples/*.bc
rm -f samples/*.elf