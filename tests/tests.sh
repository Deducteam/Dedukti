echo "run tests..."
make -C tests/OK/ all
for i in tests/KO/*.sk ; do \
    echo "on $i...  " ; \
    ./skcheck.native "$i" 2>&1 | grep ERROR ; \
done
echo "-----------------------"
echo "tests OK"
