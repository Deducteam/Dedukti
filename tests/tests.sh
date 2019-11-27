#!/bin/bash

################################ Test conventions ##########################################
#
# - Test files are prefixed with a comment describing the expected behavior.
#   This comment should have the following shape:
#     (;  TEST flag1 flag2 ... flagn  ;)
# - All tests consist in the run of the dkcheck command with given flag1 ... flagn
#   on the corresponding .dk test file.
# - Tests with comment TEST=KO n  (for n an integer error code) pass only when they:
#   - output the "[ERROR:n]" keyword.
# - Tests with comment TEST=OK pass when they:
#   - typecheck without failure.
#   - output pairs of strictly identical lines followed by the final "SUCCESS" notification.
#
# When using commands for OK-testing, the test file should first print the expected output
# using the #PRINT command.
# For instance:
#     ...
#     #PRINT "YES".      (; Expected output ;)
#     #CHECK plus 1 1, nat.
#     #PRINT "NO".       (; Expected output ;)
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
#
############################################################################################

all_tests=$(find tests -name "*.dk" | sort)
total=$(wc -w <<< "$all_tests")

find -name "*.dko" -exec rm {}

echo ""
echo "------------------------"
echo "  Running $total tests"
echo "------------------------"

passed=0

for i in $all_tests ; do
	echo -n "$i... " ;
	instructions=$(head -n 1 $i | sed -e "s/^ *(;//g" | sed -e "s/;) *$//g" | tr -s ' ')
	cmd=$(echo $instructions | cut -d ' ' -f 1-1)
	flags=$(echo $instructions | cut -s -d ' ' -f 2-)
	if bash "./tests/scripts/$cmd.sh" $flags $i;
	then
		passed=$((passed+1)) ;
		echo -e "\033[0;32mPassed\033[0m"
	else
		echo -e "\033[0;31mFailed !\033[0m"
	fi ;
done

echo "------------------------"
if [ "$passed" -eq "$total" ]
then
	echo -e "\033[0;32mPassed: $passed / $total\033[0m"
	exit 0
else
	echo -e "\033[0;31mPassed: $passed / $total\033[0m"
	exit 1
fi
