echo "run tests..."
for i in tests/OK/*.dk ; do \
    echo "on $i...  " ; \
    ./dkcheck.native "$i" || exit 1; \
done
for i in tests/KO/*.dk ; do \
    echo "on $i...  " ; \
    ./dkcheck.native "$i" 2>&1 | grep ERROR ; \
done
echo "-----------------------"
echo "tests OK"
