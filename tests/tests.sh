passed=0
total=0

echo "Running tests..."
for i in tests/OK/*.dk ; do
	total=$((total+1)) ;
    echo "on $i...  " ;
    if ./dkcheck.native "$i" 2>&1 ;
	then
		passed=$((passed+1)) ;
		echo "Passed."
	else
		echo "Failed !"
	fi ;
done
for i in tests/KO/*.dk ; do
	total=$((total+1)) ;
    echo "on $i...  " ;
#	lines=$(wc -l $i | cut -d" " -f1) ;
#	"error.* line:$lines "
    if ./dkcheck.native -nc "$i" 2>&1 | grep -i -q "error" ;
	then
		passed=$((passed+1)) ;
		echo "Passed."
	else
		echo "Failed !"
	fi
done
echo "-----------------------"
echo "Passed: $passed / $total"
if [ "$passed" -eq "$total" ]
then
	echo "tests OK"
	exit 0
else
	echo "tests KO !"
	exit 1
fi
