#!/bin/bash

DKCHECK="$(pwd)/../../dk.native check"
DKDEP="$(pwd)/../../dk.native dep"
DKFLAGS="-q"

SRC="https://deducteam.github.io/data/libraries/matita.tar.gz"
DIR="matita"

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
  # Escaping the injective function, since it became a keyword since last update of the generator.
  sed -i 's/\([ \.]\)injective\([ \.\n]\)/\1{|injective|}\2/g' $DIR/*.dk
  # The options given to dkcheck also changed
  sed -i 's/-nl//g' $DIR/Makefile
  echo "OK"
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
