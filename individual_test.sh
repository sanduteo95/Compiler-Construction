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
OUTPUT=$(./exp_test.native $1 $2)
if "${OUTPUT: -4}" -eq "true" ; then
	echo -e "${GREEN}passed${NC}"
else
	echo -e "${RED}failed${NC}"
fi
