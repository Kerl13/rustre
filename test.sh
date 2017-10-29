#!/bin/bash


RUSTRE=$1
if [ "_$RUSTRE" == "_" -o ! -f "$RUSTRE" ]
then
    echo "usage: $0 RUSTRE"
    echo "  RUSTRE  path to rustre"
    exit 1
fi


function compile {
    ./$RUSTRE $1 $2 > /dev/null
}


TOTAL_TESTS=0
ERRORS=0

for file in tests/*.lus
do
    TOTAL_TESTS=$((TOTAL_TESTS+1))
    if compile $file main
    then
        printf "."
    else
        ERRORS=$((ERRORS+1))
    fi
done

echo
echo "Ran $TOTAL_TESTS tests"
if [ $ERRORS -ne 0 ]
then
    echo "Fail: $ERRORS errors"
else
    echo OK
fi
