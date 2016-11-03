#!/bin/bash
if [[ $# -eq 0 ]] ; then
    echo 'You need to input a file name.'
    exit 0
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
LENGTH=${#1}
echo -n "${1:11:$LENGTH-11}: "

echo ""
OUTPUT1=$(./exp_test.native $1 $2)
# echo "NON-OPT: ${OUTPUT1}"
IFS=' ' read -r -a array <<< ${OUTPUT1}
length=${#array[@]}
STEPS1=${array[${length}-3]}
RESULT1=${array[${length}-2]}
TIME1=${array[${length}-1]}

echo "Non-ptimised"
echo "Time : ${TIME1} "
echo "Number of steps: ${STEPS1} "
printf "Test: "
if "${RESULT1: -4}" -eq "true" ; then
    echo -e "${GREEN}passed${NC}"
else
    echo -e "${RED}failed${NC}"
fi

echo ""

OUTPUT2=$(./exp_test.native -o $1 $2)
# echo "OPT: ${OUTPUT2}"
IFS=' ' read -r -a array <<< ${OUTPUT2}
length=${#array[@]}
STEPS2=${array[${length}-3]}
RESULT2=${array[${length}-2]}
TIME2=${array[${length}-1]}

echo "Optimised"
echo "Time : ${TIME2} "
echo "Number of steps: ${STEPS2} "
printf "Test: "
if "${RESULT2: -4}" -eq "true" ; then
	echo -e "${GREEN}passed${NC}"
else
	echo -e "${RED}failed${NC}"
fi
echo ""
