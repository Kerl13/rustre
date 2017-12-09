#!/bin/bash

# One tick of the base clock
DT=0
if [ "_$1" != "_" ]
then
    DT=$1
fi

while true
do
  printf "1\n"
  sleep $DT
done
