#!/bin/bash


BIN="$(pwd)/../dkcheck.native -q"
SRC="https://github.com/Deducteam/Libraries/archive/master.zip"
DIR="Libraries-master"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  if [[ "$1" = "fullclean" ]]; then
    rm -f Libraries-master.zip
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
  if [[ ! -f Libraries-master.zip ]]; then
    echo -n "  - downloading...      "
    wget -q ${SRC}
    mv master.zip Libraries-master.zip
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...       "
  unzip Libraries-master.zip
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

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
