#!/bin/bash 
if [[ $# -eq 0 ]] ; then
    echo 'You need to input a file name.'
    exit 0
fi 

echo "Test ($1): "
./exp_test.native $1
