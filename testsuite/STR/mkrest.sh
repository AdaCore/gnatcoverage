#!/bin/sh

# This script is invoked by genbundle.py during qualification material
# production. It's goal is to to produce the REST corresponding to the STR
# report for a testsuite run that occured in our parent subdir.

# $1 is the level for which qualification material is being produced.
# This will have to match a --qualif-level passed to run the testsuite.
dolevel=$1

# Setup PATH to expose gnatpython
PATH=/home/hainque/local/bin:$PATH

python genrest.py --testsuite-dir=../ --dolevel=$dolevel
