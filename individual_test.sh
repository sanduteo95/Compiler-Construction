#!/bin/bash

if [ $1 == "-p" ] || [ $1 == "-g" ] ||  [ $1 == "-i" ] ||  [ $1 == "-e" ]  ; then
    if [[ $# == 2 ]] ; then
        file="${2: 0: 11}inputs/${2: 11: -4}.txt"
        if [ ! -f $file ]; then
            ./exp_test.native $1 $2 2> /dev/null
        else
            cat $file | ./exp_test.native $1 $2 2> /dev/null
        fi
    else
        file="${3: 0: 11}inputs/${3: 11: -4}.txt"
        if [ ! -f $file ]; then
            ./exp_test.native $1 $2 $3 2> /dev/null
        else
            cat $file | ./exp_test.native $1 $2 $3 2> /dev/null
        fi
    fi
elif [ $1 == "-s" ] ; then
    if [[ $# == 2 ]] ; then
        file="${2: 0: 11}inputs/${2: 11: -4}.txt"
        if [ ! -f $file ]; then
            sh run.sh $1 $2
        else
            cat $file | sh run.sh $1 $2
        fi
    else
        file="${3: 0: 11}inputs/${3: 11: -4}.txt"
        if [ ! -f $file ]; then
            sh run.sh $1 $2 $3
        else
            cat $file | sh run.sh $1 $2 $3
        fi
    fi
fi
