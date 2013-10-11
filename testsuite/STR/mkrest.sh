#!/bin/sh

# This script is invoked by genbundle.py during qualification material
# production. It's goal is to to produce the REST corresponding to the STR
# report for a testsuite run that occured in our parent subdir.

# $1 is the level for which qualification material is being produced.
# This will have to match a --qualif-level passed to run the testsuite.
dolevel=$1

# Setup PATH to expose the same gnatpython as the one that was used to run the
# testsuite. This is required to make sure that data dumps are read correctly
# by genrest.py (e.g. agree on the pickle format).

PATH=$(dirname $(cat ../python_bin.dump)):$PATH

python genrest.py --testsuite-dir=../ --dolevel=$dolevel
