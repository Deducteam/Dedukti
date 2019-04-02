#!/bin/bash

BIN="$(pwd)/../dkcheck.native -q"
SRC="https://deducteam.github.io/data/libraries/matita.tar.gz"
DIR="matita-light"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  if [[ "$1" = "fullclean" ]]; then
    rm -f matita.tar.gz
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
  if [[ ! -f matita.tar.gz ]]; then
    echo -n "  - downloading...      "
    wget -q ${SRC}
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...       "
  tar xf matita.tar.gz
  mv matita $DIR
  # Editing factorial file : turning le_fact_10 into an axiom
  sed -i '30816,33252d'                      $DIR/matita_arithmetics_factorial.dk
  sed -i '30815s/.*/\./'                     $DIR/matita_arithmetics_factorial.dk
  sed -i 's/def le_fact_10 :/le_fact_10 :/'  $DIR/matita_arithmetics_factorial.dk
  echo "OK"
fi

cd ${DIR}
if [[ $TIME = "" ]]; then
	export TIME="Finished in %E at %P with %MKb of RAM"
fi

# Run the actual checks.
if [[ $OUT = "" ]]; then
	\time make "DKCHECK=$BIN"
else
	\time -a -o $OUT make "DKCHECK=$BIN"
fi
