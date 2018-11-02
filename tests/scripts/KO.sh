#!/bin/bash

err_code=$1
shift

./dkcheck.native -I tests/KO -nc $@ 2>&1 | grep -i -q "^\[ERROR:$err_code\]"
