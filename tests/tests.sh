#!/bin/bash

# Test convention:
# - Tests in the KO folder pass when they output the "error" keyword by either
#   - raising an error (syntax or other)
#   - terminating with a failure to typecheck
# - Tests in the OK folder pass when they both
#   - typecheck without failure.
#   - output pairs of strictly identical lines followed by the final "SUCCESS" notification.
#
# When using commands for testing, the test file should first print the expected output
# using the #PRINT command.
# For instance:
#     ...
#     #PRINT "YES".      (; Expected output ;)
#     #CHECK plus 1 1, nat.
#     #PRINT "NO".      (; Expected output ;)
#     #CONV plus 1 1, 3.
# Should output
#     YES
#     YES
#     NO
#     NO
#     SUCCESS File ... was successfully checked.
# Which is accepted as a successful output and validates the test.
#
# Tests failing with an internal error (segfault) will break the test script.

passed=0
total=0

for i in tests/OK/*.dk ; do
	total=$((total+1)) ;
    echo -n "$i..." ;
    if ./dkcheck.native -q -nc "$i" 2>&1 | uniq -c | egrep  "^ *[0-9]*(1|3|5|7|9) .*" | egrep -v -q "^ *1 SUCCESS.*" ;
	then
		echo -e "\033[0;31mKO\033[0m"
	else
		passed=$((passed+1)) ;
		echo -e "\033[0;32mOK\033[0m"
	fi ;
done

for i in tests/KO/*.dk ; do
	total=$((total+1)) ;
    echo -n "$i...  " ;
    if ./dkcheck.native -nc "$i" 2>&1 | grep -i -q "error" ;
	then
		passed=$((passed+1)) ;
		echo -e "\033[0;32mKO\033[0m"
	else
		echo -e "\033[0;31mOK\033[0m"
	fi
done

echo "-----------------------"
if [ "$passed" -eq "$total" ]
then
	echo -e "\033[0;32mPassed: $passed / $total\033[0m"
	exit 0
else
	echo -e "\033[0;31mPassed: $passed / $total\033[0m"
	exit 1
fi
