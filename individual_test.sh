#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo "Not enough arguments."
    exit 0
fi

LENGTH=${#2}
echo "${2:11:$LENGTH-11}: "
OUTPUT=$(./exp_test.native $1 $2)
if [ $1 == "-p" ] || [ $1 == "-g" ] || [ $1 == "-i" ]; then
    echo $OUTPUT
fi

if [ $1 == "-e" ] || [ $1 == "-o" ] ; then
    IFS=', ' read -r -a array <<< ${OUTPUT}
    length=${#array[@]}
    TIME=${array[${length}-1]}
    STEPS=${array[${length}-2]}
    RESULT=${array[${length}-3]}
    echo "  Time : ${TIME} "
    echo "  Number of steps: ${STEPS} "
    echo "  Result: ${RESULT}"
    EXPECTED="`cat ${3}`"
    if [ $RESULT == $EXPECTED ] ; then
        echo "  Test: PASSED"
    else
        echo "  Test: FAILED"
    fi
fi
echo ""
