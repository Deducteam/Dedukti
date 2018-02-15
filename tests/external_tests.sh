TMPDIR="/tmp/dedukti_tmp_testdir/"
DKCHECK="$(pwd)/dkcheck.native"
DKDEP="$(pwd)/dkdep.native"

export DKCHECK
export DKDEP

# Creating temporary directory and moving there.
echo 'Creating temporary directory "$TMPDIR"'
rm -rf $TMPDIR
mkdir $TMPDIR
cd $TMPDIR

# Focalide library.

echo "Running Focalide"
wget https://deducteam.github.io/data/libraries/focalide.tar.gz
tar zxvf focalide.tar.gz
cd focalide_dks
make all
cd ..

# Matita library.

echo "Running matita"

wget https://deducteam.github.io/data/libraries/matita.tar.gz
tar zxvf matita.tar.gz
cd matita
rm matita_arithmetics_factorial.dk
rm matita_arithmetics_binomial.dk
rm matita_arithmetics_chebyshev_*.dk
rm matita_arithmetics_chinese_reminder.dk
rm matita_arithmetics_congruence.dk
rm matita_arithmetics_fermat_little_theorem.dk
rm matita_arithmetics_gcd.dk
rm matita_arithmetics_ord.dk
rm matita_arithmetics_primes.dk
make all
cd ..

# Dklib.

echo "Running dklib-master"

git clone https://github.com/rafoo/dklib.git dklib
cd dklib
make
