#!/bin/bash

COMPILER=$1
DEFAULT_MAIN=check

fails=0
total=0

function compile {
  local ifile=$1
  local opts=$2
  set -f
  fst=$(head -n 1 $ifile)
  [[ $fst =~ ^"/* main = "([a-zA-Z0-9_]+)" */"$ ]]
  set +f
  MAIN=${BASH_REMATCH[1]}
  if [ "_$MAIN" != "_" ]
  then
    $COMPILER $opts $ifile $MAIN > /dev/null
  else
    $COMPILER $opts $ifile $DEFAULT_MAIN > /dev/null
  fi
}

function parse_error {
  set -f
  snd=$(sed '2q;d' $1)
  [[ $snd =~ ^"/* error = "(.*)" */"$ ]]
  set +f
  echo ${BASH_REMATCH[1]}
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
    echo
  fi
  total=$((total+1))
done

echo
echo "Running positive tests with the extension"
for file in auto/*.lus
do
  compile $file "-ext"
  if [ $? -eq 0 ]
  then
    printf "."
  else
    fails=$((fails+1))
    echo
  fi
  total=$((total+1))
done

echo
echo "Checking compatibility between standard syntax and extended syntax"
for file in *.lus
do
  compile $file "-ext"
  if [ $? -eq 0 ]
  then
    printf "."
  else
    fails=$((fails+1))
    echo
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
    echo >&2
    echo "$file should not compile. Comment at the top of te file says:" >&2
    parse_error $file >&2
    fails=$((fails+1))
  fi
  total=$((total+1))
done

echo
echo "Ran $total test"
if [ $fails -ne 0 ]
then
  echo -e "\033[0;31mErrors:\033[0m $fails"
else
  echo -e "\033[0;32mOK\033[0m"
fi
