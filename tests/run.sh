#!/bin/bash

COMPILER=$1
DEFAULT_MAIN=check

fails=0
total=0

function compile {
  set -f
  fst=$(head -n 1 $1)
  [[ $fst =~ ^"/* main = "([a-zA-Z0-9_]+)" */"$ ]]
  set +f
  MAIN=${BASH_REMATCH[1]}
  if [ "_$MAIN" != "_" ]
  then
    $COMPILER $1 $MAIN > /dev/null
  else
    $COMPILER $1 $DEFAULT_MAIN > /dev/null
  fi
}

echo "Running positive tests"
for file in *.lus
do
  compile $file
  if [ $? -eq 0 ]
  then
    printf "."
  else
    fails=$((fails+1))
  fi
  total=$((total+1))
done

echo
echo "Running negative tests"
for file in bad/*.lus
do
  compile $file 2>/dev/null
  if [ $? -ne 0 ]
  then
    printf "."
  else
    echo "$file should have failed" >&2
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
