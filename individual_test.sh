#!/bin/bash
if [[ $# -eq 0 ]] ; then
    echo 'Not enough arguments.'
    exit 0
fi

LENGTH=${#2}
echo -n "${2:11:$LENGTH-11}: "
echo ""
OUTPUT=$(./exp_test.native $1 $2 $3)
if [ $1 == "-p" ] || [ $1 == "-i" ] ; then
    echo ${OUTPUT}
fi

if [ $1 == "-e" ] || [ $1 == "-o" ] ; then
    IFS=' ' read -r -a array <<< ${OUTPUT}
    length=${#array[@]}
    STEPS=${array[${length}-3]}
    RESULT=${array[${length}-2]}
    TIME=${array[${length}-1]}
    echo "  Time : ${TIME} "
    echo "  Number of steps: ${STEPS} "
    echo "  Test: ${RESULT: -6}"
fi
echo ""
