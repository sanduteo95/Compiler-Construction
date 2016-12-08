#!/bin/bash

declare -a flags=("-p" "-e" "-i" "-s")

for flag in "${flags[@]}";
do
	echo ""
	if [[ $flag == "-p" ]] ; then
		echo "=================PARSER================="
	elif [[ "$flag" == "-e" ]]; then
		echo "================EVALUATOR================"
	elif [[ "$flag" == "-i" ]]; then
		echo "===============INTERPRETER==============="
	elif [[ "$flag" == "-s" ]]; then
		echo "================COMPILER================"
	fi
	echo ""
	sh test.sh $flag $file
	echo ""
done
