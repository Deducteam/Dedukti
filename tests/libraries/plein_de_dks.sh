#!/bin/bash

BIN="../../../dkcheck.native -q"
SRC="https://git.lsv.fr/genestier/PleinDeDk/repository/master/archive.tar.gz"
DIR="plein_de_dks"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  rm -rf PleinDeDk-*
  if [[ "$1" = "fullclean" ]]; then
    rm -f ${DIR}.tar.gz
  fi
  exit 0
fi

# Rejecting other command line arguments.
if [[ "$#" -ne 0 ]]; then
  echo "Invalid argument, usage: $0 [clean | fullclean]"
  exit -1
fi

# Prepare the library if necessary.
if [[ ! -d ${DIR} ]]; then
  # The directory is not ready, so we need to work.
  echo "Preparing the library:"

  # Download the library if necessary.
  if [[ ! -f ${DIR}.tar.gz ]]; then
    echo -n "  - downloading...      "
    wget -q ${SRC} -O ${DIR}.tar.gz
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...       "
  tar xf ${DIR}.tar.gz
  mv PleinDeDk-* ${DIR}
  echo "OK"

  # Cleaning up.
  echo -n "  - cleaning up...      "
  rm ${DIR}/.gitignore ${DIR}/removeConneries.py ${DIR}/removeTypeCtx.py
  rm -r ${DIR}/CoqModels ${DIR}/Paradoxes ${DIR}/Sudoku
  rm -r ${DIR}/TermChecker ${DIR}/Theories
  mv ${DIR}/Miscellaneous/*.dk ${DIR}
  rm -r ${DIR}/Miscellaneous
  rm ${DIR}/intBinaire.dk
  rm ${DIR}/numberBases.dk
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

# Checking function.
function check_plein_de_dks() {
  for FILE in `ls *.dk`; do
	${BIN} -nl ${FILE}
	if [ $? -ne 0 ]; then
	  echo "File ${FILE} failed !"
	  exit 1
	fi
  done
}

# Export stuff for the checking function.
export readonly BIN=${BIN}
export -f check_plein_de_dks

# Run the actual checks.
cd ${DIR}
if [[ $TIME = "" ]]; then
	export TIME="Finished in %E at %P with %MKb of RAM"
fi

if [[ $OUT = "" ]]; then
	\time bash -c "check_plein_de_dks"
else
	\time -a -o $OUT bash -c "check_plein_de_dks"
fi
