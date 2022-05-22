#!/bin/bash

set -f

shift #Remove the reset regression parameter
shift #Remove the filename (which is duplicated anyway)

OUT=$(./dk.native check --no-color "$@" 2>&1 | uniq -c | egrep "^ *[0-9]*(1|3|5|7|9) .*")
LINES=$(echo $OUT | wc -l)
SUCCESSLINES=$(echo $OUT | grep "^ *1 \[SUCCESS\].*" | wc -l)


if [ $LINES -eq 1 ] && [ $SUCCESSLINES -eq 1 ]
then
	exit 0
else
    exit 1
fi


: '
"dkcheck ..." should generate an output of pairwise identical lines followed by "[SUCCESS]...":
   a
   a
   a
   a
   b
   b
   [SUCCESS]...

"uniq -c" groups the lines and display line counts:

  4 a
  2 b
  1 [SUCCESS]...

"egrep ..." keeps odd number prefixed lines (which should be only the [SUCCESS])

   1 [SUCCESS]...

It is then check that the remaining output has exactly one line and that this line
matches the "^ *1 \[SUCCESS\].*" regexp
'
