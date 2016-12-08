#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo "You need to add a flag: <-s> to generate x86 code, <-o -s> to generate x86 after program was optimised."
    exit 0
fi

OUTPUT=""
if [[ $# -eq 2 ]] ; then
    OUTPUT=$(./exp_test.native $1 $2 2> /dev/null)
else
    OUTPUT=$(./exp_test.native $1 $2 $3 2> /dev/null)
fi

if [[ $OUTPUT != "" ]] ; then
     echo "$OUTPUT" >> program.txt

    mv program.txt program.s
    gcc -c program.s -o program.o
    gcc program.o -o program
    ./program
    if [[ $# -eq 2 ]] ; then
        mv program.s "${2: 0: 11}results/assembly/${2: 11: -4}.s"
        rm program.o
        mv program "${2: 0: 11}results/assembly/${2: 11: -4}"
    else
        mv program.s "${3: 0: 11}results/assembly/${3: 11: -4}.s"
        rm program.o
        mv program "${3: 0: 11}results/assembly/${3: 11: -4}"
    fi
else
    echo ""
fi