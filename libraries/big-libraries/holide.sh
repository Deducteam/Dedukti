#!/bin/bash

DKCHECK="$(pwd)/../../dk.native check"
DKDEP="$(pwd)/../../dk.native dep"
DKFLAGS="-q"

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
  for FILE in `find "${DIR}" -type f -name "*.dk"`; do
    if [ "${FILE}" != "${DIR}/hol.dk" ]; then
      sed -i 's/^[{]\([a-zA-Z0-9_-]*\)[}]/thm \1/g' "${FILE}"
    fi
  done
  # The options given to dkcheck also changed
  sed -i 's/-nl//g' $DIR/Makefile
  sed -i 's/-error/--error/g' $DIR/Makefile
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

cd "${DIR}" || exit 1
if [[ $TIME = "" ]]; then
	export TIME="Finished in %E at %P with %MKb of RAM"
fi

# Run the actual checks.
if [[ $OUT = "" ]]; then
	command time make DKCHECK="$DKCHECK" DKDEP="$DKDEP" DKFLAGS="$DKFLAGS"
else
	command time -a -o "$OUT" make DKCHECK="$DKCHECK" DKDEP="$DKDEP" \
	    DKFLAGS="$DKFLAGS"
fi
