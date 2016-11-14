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

    # ./exp_test.native $1 $2

    OUTPUT=$(./exp_test.native $1 $2)
    BEFORE=`cat before.txt`
    echo "$BEFORE$OUTPUT" >> program.txt
    mv program.txt program.s
    gcc -c program.s -o program.o
    gcc program.o -o program
    # rm program.s
    rm program.o
    ./program
    rm program
else
    echo "This script only works for generating x86 code."
    exit 0
fi
