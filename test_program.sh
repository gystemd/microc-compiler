dune exec bin/microcc.exe -- $1
clang a.bc bin/rt-support.c
./a.out
rm a.bc a.out