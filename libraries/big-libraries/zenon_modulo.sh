#!/bin/bash

NBWORKERS="4"

DKCHECK="$(pwd)/../../dk.native check"
DKDEP="$(pwd)/../../dk.native dep"
DKFLAGS="-q"

# This source file is not valid anymore
SRC="http://deducteam.gforge.inria.fr/lib/zenon_modulo.tar"
DIR="zenon_modulo"

# Cleaning command (clean and exit).
if [[ "$#" -eq 1 && ("$1" = "clean" || "$1" = "fullclean") ]]; then
  rm -rf ${DIR}
  if [[ "$1" = "fullclean" ]]; then
    rm -f zenon_modulo.tar
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
  if [[ ! -f zenon_modulo.tar ]]; then
    echo -n "  - downloading...           "
    wget -q ${SRC}
    echo "OK"
  fi

  # Extracting the source files.
  echo -n "  - extracting...            "
  tar xf zenon_modulo.tar
  echo "OK"

  # Renaming file.
  echo -n "  - renaming file...         "
  mv ${DIR}/logic/basics_minimal.dk ${DIR}/logic/basics.dk
  echo "OK"

  # All done.
  echo "Ready."
  echo ""
fi

# Cleaning up.
rm -rf ${DIR}/workdir
mkdir -p ${DIR}/workdir

# Preparing the theory files.
echo "preparing logic files... "
for FILE in `ls ${DIR}/logic/*.dk`; do
  MODNAME="$(basename $FILE .dk)"
  OUTFILE="${DIR}/workdir/${MODNAME}.dk"
  cat $FILE | grep -v "^#NAME" >> $OUTFILE
done

# Moving to the working directory.
cd ${DIR}/workdir

# Compiling the theory files.
echo "Compiling the theory files..."
$DKCHECK $DKFLAGS -e cc.dk        2> /dev/null
$DKCHECK $DKFLAGS -e dk_bool.dk   2> /dev/null
$DKCHECK $DKFLAGS -e dk_logic.dk  2> /dev/null
$DKCHECK $DKFLAGS -e dk_tuple.dk  2> /dev/null
$DKCHECK $DKFLAGS -e basics.dk    2> /dev/null
$DKCHECK $DKFLAGS -e zen.dk       2> /dev/null
$DKCHECK $DKFLAGS -e zen_focal.dk 2> /dev/null

# Checking function.
function check() {
  # Single file checking.
  function check_gz() {
    LIBFILE="$1"
    FILE_GZ="$(basename $1)"
    FILE_DK="$(basename $FILE_GZ .gz)"
    cp ${LIBFILE} ${FILE_GZ}
    gzip -d ${FILE_GZ}
    ${DKCHECK} ${DKFLAGS} ${FILE_DK} 2> /dev/null
    if [ $? -ne 0 ]; then
      echo -e "\033[0;31mKO\033[0m ${FILE_GZ}"
      echo "FAILED ${FILE_GZ}" >> error.log
    else
      echo -e "\033[0;32mOK\033[0m ${FILE_GZ}"
    fi
    rm -f ${FILE_DK}
  }

  export -f check_gz
  export readonly DKCHECK="${DKCHECK}"
  export readonly DKFLAGS="${DKFLAGS}"

  echo "Compiling the library files with ${NBWORKERS} processes..."
  find ../files -type f \
      | xargs -P ${NBWORKERS} -n 1 -I{} bash -c "check_gz {}"
}

# Exporting necessary things.
export readonly DKCHECK="${DKCHECK}"
export readonly DKFLAGS="${DKFLAGS}"
export readonly NBWORKERS=${NBWORKERS}
export -f check

cd ${DIR} || exit 1
if [[ $TIME = "" ]]; then
	export TIME="Finished in %E at %P with %MKb of RAM"
fi

# Run the actual checks.
if [[ $OUT = "" ]]; then
	command -v time bash -c "check"
else
	command -v time -a -o "$OUT" bash -c "check"
fi
