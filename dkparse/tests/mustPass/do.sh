#!/bin/bash

luajit='luajit-2.0.0-beta9'

for file in *.dk ; do
	#dedukti -g $file | $luajit -
	dedukti $file
done
