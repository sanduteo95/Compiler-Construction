#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo "You need to add a flag: <-i> to interpret, <-g> to generate."
    exit 0
fi

if [ $1 == "-s" ] ; then
    if [[ $# -eq 1 ]] ; then
        echo "You need to give the path to the file in the test folder."
        exit 0
    fi

    OUTPUT=$(./exp_test.native $1 $2)
    if [[ $OUTPUT != "" ]] ; then
        echo "$OUTPUT" >> program.txt

        mv program.txt program.s
        gcc -c program.s -o program.o
        gcc program.o -o program
        ./program
        mv program.s "${2: 0: 11}results/assembly/${2: 11: -4}.s"
        rm program.o
        mv program "${2: 0: 11}results/assembly/${2: 11: -4}"
    else
        echo "NotImplemented"
    fi
else
    echo "This script only works for generating x86 code."
    exit 0
fi
