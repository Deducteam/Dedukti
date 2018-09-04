#!/bin/bash

if ./dkcheck.native -q --nc $@ 2>&1 | uniq -c | egrep  "^ *[0-9]*(1|3|5|7|9) .*" | egrep -v -q "^ *1 \[SUCCESS\].*"
then
	exit 1
else
	exit 0
fi
