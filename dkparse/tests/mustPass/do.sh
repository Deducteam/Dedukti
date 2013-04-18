#!/bin/bash

for file in *.dk ; do
	echo "Test '$file':"
	dkjit -q $file && echo -e " > \e[32mSUCCESS !\e[m" || echo -e " > \e[31mFAIL !\e[m"
done
