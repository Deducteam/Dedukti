#!/bin/bash

DKCHECK="$(pwd)/../dkcheck.native"
DKDEP="$(pwd)/../dkdep.native"
DKFLAGS="-q"

SRC="https://deducteam.github.io/data/libraries/iProverModulo_dk.tar.gz"
DIR="iprover"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  rm -rf iProverModulo_dk
  rm -f iProverModulo_dk.tar.gz
  if [[ "$1" = "fullclean" ]]; then
    rm -f iprover.tar.gz
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
  if [[ ! -f iprover.tar.gz ]]; then
    echo -n "  - downloading...      "
    wget -q ${SRC} -O iprover.tar.gz
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...       "
  tar xf iprover.tar.gz
  mv iProverModulo_dk ${DIR}
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

cd ${DIR}
if [[ $TIME = "" ]]; then
	export TIME="Finished in %E at %P with %MKb of RAM"
fi

# Run the actual checks.
if [[ $OUT = "" ]]; then
	\time make DKCHECK=$DKCHECK DKDEP=$DKDEP DKFLAGS=$DKFLAGS
else
	\time -a -o $OUT make DKCHECK=$DKCHECK DKDEP=$DKDEP DKFLAGS=$DKFLAGS
fi
