#!/bin/bash

err_code=$1
shift

./dkcheck.native -nc $@ 2>&1 | grep -i -q "^\[ERROR/[a-zA-Z0-9_]*:$err_code\]"
