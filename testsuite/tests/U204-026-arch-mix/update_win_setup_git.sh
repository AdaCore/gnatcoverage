# Set up the gnatcoverage repositoy on the Windows IOD machie

set -ex

cd /Users/itmgr
if ! [ -d gnatcoverage ]
then
    git clone git-adacore:eng/cov/gnatcoverage
fi
