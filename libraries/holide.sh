#!/bin/bash

BIN="$(pwd)/../dkcheck.native -q"
SRC="https://deducteam.github.io/data/libraries/holide.tar.gz"
DIR="holide"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  if [[ "$1" = "fullclean" ]]; then
    rm -f holide.tar.gz
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
  if [[ ! -f holide.tar.gz ]]; then
    echo -n "  - downloading...      "
    wget -q ${SRC}
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...       "
  tar xf holide.tar.gz
  echo "OK"

  # Applying the changes (add "#REQUIRE hol" and fix obsolete syntax).
  echo -n "  - applying changes... "
  for FILE in `find ${DIR} -type f -name "*.dk"`; do
    if [ ${FILE} != "${DIR}/hol.dk" ]; then
      sed -i 's/^[{]\([a-zA-Z0-9_-]*\)[}]/thm \1/g' ${FILE}
    fi
  done
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

# Checking function.
function check_holide() {
  rm -f hol.dko
  ${BIN} --gen-obj hol.dk
  for FILE in `ls *.dk`; do
    if [ ${FILE} != "hol.dk" ]; then
      ${BIN} ${FILE}
    fi
  done
}

# Export stuff for the checking function.
export readonly BIN=${BIN}
export -f check_holide

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
