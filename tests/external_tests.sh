echo "Running Focalide"
wget https://deducteam.github.io/data/libraries/focalide.tar.gz
tar zxvf focalide.tar.gz
cd focalide_dks
make all DKCHECK=../dkcheck.native DKDEP=../dkdep.native
cd ..
echo "Running matita"
wget https://deducteam.github.io/data/libraries/matita.tar.gz
tar zxvf matita.tar.gz
cd matita
rm matita_arithmetics_factorial.dk matita_arithmetics_binomial.dk matita_arithmetics_chebyshev_*.dk  matita_arithmetics_chinese_reminder.dk  matita_arithmetics_congruence.dk  matita_arithmetics_fermat_little_theorem.dk  matita_arithmetics_gcd.dk  matita_arithmetics_ord.dk  matita_arithmetics_primes.dk
make all DKCHECK=../dkcheck.native DKDEP=../dkdep.native
cd ..
echo "Running dklib-master"
git clone https://github.com/rafoo/dklib.git dklib
cd dklib
make
