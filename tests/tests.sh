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

all_dk_tests=$(find tests -name "*.dk" | sort)
all_sk_tests=$(find tests -name "*.sk" ! -name "*builtins.sk" | sort)
total_dk=$(wc -w <<< "$all_dk_tests")
total_sk=$(wc -w <<< "$all_sk_tests")

find -name "*.dko" -exec rm {}

echo ""
echo "------------------------"
echo "  Running $total tests"
echo "------------------------"

passed=0

for i in $all_dk_tests ; do
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

pass=0

echo "------------------------"
if [ "$passed" -eq "$total_dk" ]
then
    echo -e "\033[0;32mPassed: $passed / $total_dk\033[0m"
    pass=0
else
	echo -e "\033[0;31mPassed: $passed / $total_dk\033[0m"
    pass=1
fi

passed=0

echo -n "Check builtins..." ;
./dkcheck.native -nc -e --coc tests/sukerujo/builtins.sk
for i in $all_sk_tests ; do
	echo -n "$i... " ;
	instructions=$(head -n 1 $i | sed -e "s/^ *(;//g" | sed -e "s/;) *$//g" | tr -s ' ')
	cmd=$(echo $instructions | cut -d ' ' -f 1-1)
	flags=$(echo $instructions | cut -s -d ' ' -f 2-)
	if bash "./tests/scripts/$cmd.sh" -I tests/builtins $flags $i;
	then
		passed=$((passed+1)) ;
		echo -e "\033[0;32mPassed\033[0m"
	else
		echo -e "\033[0;31mFailed !\033[0m"
	fi ;
done

echo "------------------------"
if [ "$passed" -eq "$total_sk" ]
then
	echo -e "\033[0;32mPassed: $passed / $total_sk\033[0m"
	pass=0 || pass
else
	echo -e "\033[0;31mPassed: $passed / $total_sk\033[0m"
	pass=1
fi

exit $pass
