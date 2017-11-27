#!/bin/bash

COMPILER=$1
MAIN_NODE=check

fails=0
total=0


for file in *.lus
do
  $COMPILER $file $MAIN_NODE > /dev/null
  if [ $? -eq 0 ]
  then
    printf "."
  else
    fails=$((fails+1))
  fi
  total=$((total+1))
done

echo
echo "Ran $total test"
if [ $fails -ne 0 ]
then
  echo "Errors: $fails"
else
  echo OK
fi
