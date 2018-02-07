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


echo "Running tests..."

passed=0
total=0

for i in tests/OK/*.dk ; do
	total=$((total+1)) ;
    echo "on $i...  " ;
    if ./dkcheck.native -nc "$i" 2>&1 | uniq -c | egrep  "^ *[0-9]*(1|3|5|7|9) .*" | egrep -v -q "^ *1 SUCCESS.*" ;
	then
		echo "\033[0;31mFailed !\033[0m"
	else
		passed=$((passed+1)) ;
		echo "\033[0;32mPassed.\033[0m"
	fi ;
done

for i in tests/KO/*.dk ; do
	total=$((total+1)) ;
    echo "on $i...  " ;
    if ./dkcheck.native -nc "$i" 2>&1 | grep -i -q "error" ;
	then
		passed=$((passed+1)) ;
		echo "\033[0;32mPassed.\033[0m"
	else
		echo "\033[0;31mFailed !\033[0m"
	fi
done

echo "-----------------------"
echo "Passed: $passed / $total"
if [ "$passed" -eq "$total" ]
then
	echo "\033[0;32mtests OK\033[0m"
	exit 0
else
	echo "\033[0;31mtests KO !\033[0m"
	exit 1
fi
