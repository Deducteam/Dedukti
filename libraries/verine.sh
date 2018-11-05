#!/bin/bash

BIN="$(pwd)/../dkcheck.native -q"
SRC="https://deducteam.github.io/data/libraries/verine.tar.gz"
DIR="verine"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  if [[ "$1" = "fullclean" ]]; then
    rm -f verine.tar.gz
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
  if [[ ! -f verine.tar.gz ]]; then
    echo -n "  - downloading...      "
    wget -q ${SRC}
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...       "
  tar xf verine.tar.gz
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

# Checking function.
function check_verine() {
  rm -f logic.dko
  ${BIN} --gen-obj logic.dk
  for FILE in `ls SEQ*.dk`; do
    ${BIN} ${FILE}
  done
}

# Export stuff for the checking function.
export readonly BIN=${BIN}
export -f check_verine

# Run the actual checks.
cd ${DIR}
if [[ $TIME = "" ]]; then
	export TIME="Finished in %E at %P with %MKb of RAM"
fi

if [[ $OUT = "" ]]; then
	\time make "DKCHECK=$BIN"
else
	\time -a -o $OUT make "DKCHECK=$BIN"
fi
