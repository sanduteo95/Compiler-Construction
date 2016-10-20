#!/bin/bash 

echo "-------Tests from $1-------"
for entry in "test/$1"/*
do
	if [ "${entry: -4}" == ".txt" ]; then
		sh individual_test.sh $entry "test/$1/results/${entry: 11: ${#entry}-11}"
	fi
done
