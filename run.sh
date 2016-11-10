#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo "You need to add a flag: <-i> to interpret, <-g> to generate."
    exit 0
fi

if [ $1 == "-i" ] || [ $1 == "-g" ] ; then
    if [[ $# -eq 1 ]] ; then
        echo "You need to give the path to the file in the test folder."
        exit 0
    fi

    ./exp_test.native $1 "test/${2}"
else
    echo "This script only works for interpreting and generating."
    exit 0
fi
