#!/bin/bash

TMPDIR=/tmp/dedukti_tmp_testdir/
DKCHECK=$(pwd)/dkcheck.native
DKDEP=$(pwd)/dkdep.native

export DKCHECK
export DKDEP

# Creating temporary directory and moving there.
echo 'Creating temporary directory "$TMPDIR"'
rm -rf $TMPDIR
mkdir $TMPDIR
cd $TMPDIR

# Focalide library.

#echo "#### Running Focalide ##############################################"
#wget -q https://deducteam.github.io/data/libraries/focalide.tar.gz
#tar zxf focalide.tar.gz
#make -C focalide_dks

# Matita library.

echo "#### Running Matita ################################################"

wget -q https://deducteam.github.io/data/libraries/matita.tar.gz
tar zxf matita.tar.gz
rm matita/matita_arithmetics_factorial.dk
rm matita/matita_arithmetics_binomial.dk
rm matita/matita_arithmetics_chebyshev_*.dk
rm matita/matita_arithmetics_chinese_reminder.dk
rm matita/matita_arithmetics_congruence.dk
rm matita/matita_arithmetics_fermat_little_theorem.dk
rm matita/matita_arithmetics_gcd.dk
rm matita/matita_arithmetics_ord.dk
rm matita/matita_arithmetics_primes.dk
make -C matita

# Dklib.

echo "#### Running DKlib #################################################"

git clone https://github.com/rlepigre/dklib.git dklib
make -C dklib
