#!/bin/bash

flag=$1

for folder in "test"/*
do
	echo "-------Tests from ${folder: 5: 10}-------"
	for file in "${folder}"/*
	do
		if [ "${file: -4}" == ".txt" ]; then
			sh individual_test.sh $flag $file "${folder}/results/${file: 11: ${#file}-11}"
		fi
	done
done
