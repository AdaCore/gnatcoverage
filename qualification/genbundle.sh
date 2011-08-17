# Quick script to build the GNATcoverage qualification material items
# and bundle them together.

# There are three components (aka documents): PLANS, TORs, and STR (Software
# Test Results)

# NOTE: Beware that the testresults are obtained by running the testsuite,
# with the compiler determined by your PATH.

# Expect two arguments:

# 1- name of a directory where all the operations are performed (where
#    artifact sources are extracted, built, ...). This directory must not
#    exist when the script starts. The script will error out otherwise.

# 2- Base name of the bundle to be created, as a .zip file deposited in the
#    temporary directory.

if [ $# != 2 ]
then
    echo "usage: $0 <TMPROOT> <BUNDLENAME>"
    echo "!! Make sure to have the proper toolchain on PATH !!"
    exit 1
fi

# -----------
# -- SETUP --
# -----------

# Setup the temporary dir. Make sure we will start afresh ...

TMPROOT=$1

if [ -e "$TMPROOT" ]
then
    echo "TMPROOT $TMPROOT exists already"
    exit 1
fi

mkdir $TMPROOT
if [ ! -d $TMPROOT ]
then
    echo "creation of $TMPROOT failed somehow"
    exit 1
fi

shift
BUNDLENAME=$1

# Get there and acquire an absolute reference so we can
# get back easily as needed

cd $TMPROOT
TMPROOT=$PWD

# Where we place the intermediate documents until we build the toplevel index

PACKROOT=$TMPROOT/ITEMS
mkdir -p $PACKROOT

# Checkout "sources"

cd $TMPROOT

OPENDO_SVN=svn://scm.forge.open-do.org/scmrepos/svn

svn checkout -q $OPENDO_SVN/couverture/trunk/couverture

# -------------------------
# -- BUILDS TOR document --
# -------------------------

cd $TMPROOT/couverture/qualification/tor/scripts
make

mv build/html $PACKROOT/TOR

# -------------------------
# -- BUILDS STR document --
# -------------------------

# This incurs running the testsuite first. See testsuite.py for the
# role of --qualif-level

cd $TMPROOT/couverture/testsuite

ln -s ../tools/xcov/examples/support support
svn checkout -q $OPENDO_SVN/gnatpython/trunk/gnatpython  support/gnatpython
export PYTHONPATH=`pwd`/support/gnatpython

./testsuite.py --target=ppc-elf --disable-valgrind --qualif-level=doA -j6

# Then build the STR report 

cd qreport
make html

mv build/html $PACKROOT/STR

# ---------------------------
# -- BUILDS PLANS document --
# ---------------------------

cd $TMPROOT

mkdir $PACKROOT/PLANS
cp couverture/qualification/plans/plans.pdf $PACKROOT/PLANS

# ??? to be completed

# ------------------------------------------------------
# -- BUILDS TOPLEVEL index and bundle things together --
# ------------------------------------------------------

cd $TMPROOT/couverture/qualification/index
make html

INDEXROOT=${BUNDLENAME}  # relative to TMPROOT

if [ -e $TMPROOT/$INDEXROOT ]
then
   echo "INDEXROOT already there, err!"
   exit 1
fi

# Create our toplevel tree by renaming the index html product
mv build/html $TMPROOT/$INDEXROOT

# Reach there, move items and zip
cd $TMPROOT
mv $PACKROOT $INDEXROOT

zip -q -r $INDEXROOT.zip $INDEXROOT

