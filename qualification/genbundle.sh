# Quick script to build the GNATcoverage qualification material items
# and bundle them together.

# There are three components (aka documents): PLANS, TORs, and STR (Software
# Test Results)

# Expect a single argument, name of a directory where all the operations are
# performed (where artifact sources are extracted, built, ...)

if [ $# != 1 ]
then
    echo "usage: $0 <ROOT>"
    exit 1
fi

# -----------
# -- SETUP --
# -----------

# Setup the temporary dir. Make sure we will start afresh ...

ROOT=$1

if [ -e "$ROOT" ]
then
    echo "ROOT $ROOT exists already"
    exit 1
fi

mkdir $ROOT
if [ ! -d $ROOT ]
then
    echo "creation of $ROOT failed somehow"
    exit 1
fi

# Get there and acquire an absolute reference so we can
# get back easily as needed

cd $ROOT
ROOT=$PWD

# Where we place the build products in the end

PACKROOT=$ROOT/COUVERTURE
mkdir -p $PACKROOT

# Checkout "sources"

cd $ROOT

OPENDO_SVN=svn://scm.forge.open-do.org/scmrepos/svn

svn checkout -q $OPENDO_SVN/couverture/trunk/couverture

# -------------------------
# -- BUILDS TOR document --
# -------------------------

cd $ROOT/couverture/qualification/tor/scripts
make

TORDIR=$PWD/build
ln -s $TORDIR $PACKROOT/TOR

# -------------------------
# -- BUILDS STR document --
# -------------------------

# This incurs running the testsuite first. See testsuite.py for the
# role of --qualif-level

cd $ROOT/couverture/testsuite

ln -s ../tools/xcov/examples/support support
svn checkout $OPENDO_SVN/gnatpython/trunk/gnatpython  support/gnatpython
export PYTHONPATH=`pwd`/support/gnatpython

./testsuite.py --target=ppc-elf --disable-valgrind --qualif-level=doA -j6

# Then build the STR report 

make -C qreport html

STRDIR=$PWD/qreport/build
ln -s $STRDIR $PACKROOT/STR

# ---------------------------
# -- BUILDS PLANS document --
# ---------------------------

cd $ROOT

# to be completed

