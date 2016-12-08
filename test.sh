#!/bin/bash

RED='\033[1;36m'
GREEN='\033[0;31m'
NC='\033[0m' # No Color

TOTAL=0
FAILED=0

for folder in "test"/*
do
	echo "--------Tests from ${folder: 5: 10}--------"
	for file in "${folder}"/*
	do
		TOTAL=$((TOTAL+1))
		if [ "${file: -4}" == ".txt" ]; then
			LENGTH=${#file}
			echo "${file:11:$LENGTH-11}: "

			OUTPUT1=$(sh individual_test.sh $1 $file)
			OUTPUT2=$(sh individual_test.sh $1 -o $file)

			EXPECTED="`cat "${folder}/results/${file: 11: ${#file}-11}"`"

			if [ $1 == "-e" ] ; then
				if [[ $OUTPUT1 != "" ]] ; then
					IFS=', ' read -r -a array1 <<< ${OUTPUT1}
					length=${#array1[@]}
					RESULT1=${array1[${length}-3]}
					TIME1=${array1[${length}-1]}
					STEPS1=${array1[${length}-2]}
				else
					RESULT1=""
					TIME1=""
					STEPS1=""
				fi

				if [[ $OUTPUT2 != "" ]] ; then
					IFS=', ' read -r -a array2 <<< ${OUTPUT2}
					length=${#array2[@]}
					RESULT2=${array2[${length}-3]}
					TIME2=${array2[${length}-1]}
					STEPS2=${array2[${length}-2]}
				else
					RESULT2=""
					TIME2=""
					STEPS2=""
				fi

				if [[ $RESULT1 == $EXPECTED && "$EXPECTED" != "" ]] || [[ $EXPECTED == "pointer" ]] ; then
					echo -e "  Non-optimised result: ${GREEN}PASSED${NC}"
					echo "		${STEPS1} step(s)"
					echo "		${TIME1} seconds"
				else
					FAILED=$((FAILED+1))
					echo -e "  Non-optimised result: ${RED}FAILED${NC}"
				fi
				if [[ $RESULT2 == $EXPECTED && "$EXPECTED" != "" ]] || [[ $EXPECTED == "pointer" ]] ; then
					echo -e "  Optimised result: ${GREEN}PASSED${NC}"
					echo "		${STEPS2} step(s)"
					echo "		${TIME2} seconds"
				else
					FAILED=$((FAILED+1))
					echo -e "  Optimised result: ${RED}FAILED${NC}"
				fi
			elif [ $1 == "-i" ] || [ $1 == "-s" ] ; then
				if [[ $OUTPUT1 != "" ]] ; then
					IFS=', ' read -r -a array1 <<< ${OUTPUT1}
					length=${#array1[@]}
					RESULT1=${array1[${length}-1]}
				else
					RESULT1=""
				fi

				if [[ $OUTPUT2 != "" ]] ; then
					IFS=', ' read -r -a array2 <<< ${OUTPUT2}
					length=${#array2[@]}
					RESULT2=${array2[${length}-1]}
				else
					RESULT2=""
				fi

				if [[ $RESULT1 == $EXPECTED && "$EXPECTED" != "" ]] || [[ $RESULT1 == 1 && "$EXPECTED" == "true" ]] || [[ $RESULT1 == 0 && "$EXPECTED" == "false" ]] || [[ "$EXPECTED" == "pointer" ]] ; then
					echo -e "  Non-optimised result: ${GREEN}PASSED${NC}"
				else
					FAILED=$((FAILED+1))
					echo -e "  Non-optimised result: ${RED}FAILED${NC}"
				fi
				if [[ $RESULT2 == $EXPECTED && "$EXPECTED" != "" ]] || [[ $RESULT2 == 1 && "$EXPECTED" == "true" ]] || [[ $RESULT2 == 0 && "$EXPECTED" == "false" ]] || [[ "$EXPECTED" == "pointer" ]] ; then
					echo -e "  Optimised result: ${GREEN}PASSED${NC}"
				else
					FAILED=$((FAILED+1))
					echo -e "  Optimised result: ${RED}FAILED${NC}"
				fi
			elif [ $1 == "-p" ] ; then
				OUTPUT1=''
				OUTPUT1=$(sh individual_test.sh $1 $file)
				echo "${OUTPUT1}" > ${folder}/results/parser/${file: 11: ${#file}-11}

				if [[ $OUTPUT1 != '' ]] ; then
					echo -e "  Non-optimised result: ${GREEN}DONE${NC}"
				else
					FAILED=$((FAILED+1))
					echo -e "  Non-optimised result: ${RED}DONE${NC}"
				fi

				OUTPUT2=''
				OUTPUT2=$(sh individual_test.sh $1 -o $file)
				echo "${OUTPUT2}" > ${folder}/results/parser/${file: 11: ${#file}-7}_opt.txt
				if [[ $OUTPUT2 != '' ]] ; then
					echo -e "  Optimised result: ${GREEN}DONE${NC}"
				else
					FAILED=$((FAILED+1))
					echo -e "  Optimised result: ${RED}DONE${NC}"
				fi
			elif [ $1 == "-g" ] ; then
				OUTPUT1=''
				OUTPUT1=$(sh individual_test.sh $1 $file)
				echo "${OUTPUT1}" > ${folder}/results/generator/${file: 11: ${#file}-11}
				if [[ $OUTPUT1 != "error" ]] ; then
					echo -e "  Non-optimised result: ${GREEN}DONE${NC}"
				else
					FAILED=$((FAILED+1))
					echo -e "  Non-optimised result: ${RED}DONE${NC}"
				fi

				OUTPUT2=''
				OUTPUT2=$(sh individual_test.sh $1 -o $file)
				echo "${OUTPUT2}" > ${folder}/results/generator/${file: 11: ${#file}-7}_opt.txt
				if [[ $OUTPUT2 != "error" ]] ; then
					echo -e " Optimised result: ${GREEN}DONE${NC}"
				else
					FAILED=$((FAILED+1))
					echo -e "  Optimised result: ${RED}DONE${NC}"
				fi
			fi
		fi
	done
done

echo ""
echo "=================RESULT================="
PASSED=$((TOTAL-FAILED))
echo "	${PASSED}/${TOTAL} tests passed"
echo ""
