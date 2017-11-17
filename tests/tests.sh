echo "run tests..."
for i in tests/OK/*.sk ; do \
    echo "on $i...  " ; \
    ./skcheck.native "$i" || exit 1; \
done
for i in tests/KO/*.sk ; do \
    echo "on $i...  " ; \
    ./skcheck.native "$i" 2>&1 | grep ERROR ; \
done
echo "-----------------------"
echo "tests OK"
