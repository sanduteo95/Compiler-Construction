#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo "Not enough arguments."
    exit 0
fi

LENGTH=${#2}
echo "${2:11:$LENGTH-11}: "
if [ $1 == "-p" ] || [ $1 == "-g" ] ; then
    ./exp_test.native $1 $2
fi

RED='\033[1;36m'
GREEN='\033[0;31m'
NC='\033[0m' # No Color

if [ $1 == "-i" ] ; then
    OUTPUT=$(./exp_test.native $1 $2)
    IFS=', ' read -r -a array <<< ${OUTPUT}
    RESULT=${array[${length}-1]}
    EXPECTED="`cat ${3}`"
    echo "  Result: ${RESULT}"
    echo "  Expected: ${EXPECTED}"
    if [[ $RESULT == $EXPECTED ]]
    then
        echo -e "  Test: ${GREEN}PASSED${NC}"
    else
        if [[ $EXPECTED == "false" && $RESULT -le 0 ]] || [[ $EXPECTED == "true" && $RESULT -gt 0 ]] || [[ $EXPECTED == "pointer" ]]
        then
            echo -e "  Test: ${GREEN}PASSED${NC}"
        else
            echo -e "  Test: ${RED}FAILED${NC}"
        fi
    fi
fi

if [ $1 == "-e" ] || [ $1 == "-o" ] ; then
    OUTPUT=$(./exp_test.native $1 $2)
    IFS=', ' read -r -a array <<< ${OUTPUT}
    length=${#array[@]}
    TIME=${array[${length}-1]}
    STEPS=${array[${length}-2]}
    RESULT=${array[${length}-3]}
    echo "  Time : ${TIME} "
    echo "  Number of steps: ${STEPS} "
    echo "  Result: ${RESULT}"
    EXPECTED="`cat ${3}`"
    if [ "$RESULT" == "$EXPECTED" ] ; then
        echo -e "  Test: ${GREEN}PASSED${NC}"
    else
        echo -e "  Test: ${RED}FAILED${NC}"
    fi
fi

if [ $1 == "-s" ] ; then
    OUTPUT=$(sh run.sh $1 $2)
    IFS=', ' read -r -a array <<< ${OUTPUT}
    RESULT=${array[${length}-1]}
    EXPECTED="`cat ${3}`"
    echo "  Result: ${RESULT}"
    echo "  Expected: ${EXPECTED}"
    if [[ $RESULT == $EXPECTED ]]
    then
        echo -e "  Test: ${GREEN}PASSED${NC}"
    else
        if [[ $EXPECTED == "false" && $RESULT -le 0 ]] || [[ $EXPECTED == "true" && $RESULT -gt 0 ]] || [[ $EXPECTED == "pointer" ]]
        then
            echo -e "  Test: ${GREEN}PASSED${NC}"
        else
            echo -e "  Test: ${RED}FAILED${NC}"
        fi
    fi
fi

echo ""
