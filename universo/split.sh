#!/bin/bash

filename="$1"
basename=""
while read -r line
do
    if [[ $line == \#NEWMODULE* ]] ;
    then
	echo $line;
	basename=`echo $line | cut -d' ' -f2 `;
	module=${basename}dk;
	echo "#NAME $basename" > $module;
    fi
    if [[ ! -z $basename ]] && [[ $line != \#* ]] ;
    then
	echo $line >> $module
    fi
done < "$filename"
