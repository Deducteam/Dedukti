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
make all DKCHECK=../dkcheck.native DKDEP=../dkdep.native
cd ..
