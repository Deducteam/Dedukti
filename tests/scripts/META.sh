#!/bin/bash

RESET_REGRESSION=$1
shift
INPUT=$1
shift
TMP=$(mktemp /tmp/$(basename INPUT)-XXXXXX)
OUT=$(./dkmeta.native $@ 2>/dev/null >> $TMP)
EXPECTED_OUTPUT="${INPUT%.dk}.output"

if [ $RESET_REGRESSION == "true" ]
then
    echo "reset regression output"
    cp $TMP $EXPECTED_OUTPUT
fi

if ! test -f "$EXPECTED_OUTPUT"
then
    echo "Regression output was not generated"
    exit 1
fi

if cmp -s "$TMP" "$EXPECTED_OUTPUT" 
then
    exit 0
else
    diff "$TMP" "$EXPECTED_OUTPUT"
    exit 1
fi


: '"dkmeta ..." should generate an output which is equal to the regression file that was committed.'
