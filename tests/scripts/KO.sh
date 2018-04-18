#!/bin/bash

./dkcheck.native -nc $@ 2>&1 | grep -i -q "error"
