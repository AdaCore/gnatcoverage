# Update windows artifacts for U204-026-arch-mix

set -ex

if [ $# -eq 0 ]; then
  echo "No arguments provided. Please provide the name of the gnatcov branch"
  exit 1
fi

# Create a sandbox
cd /Users/itmgr
if ! [ -d gnatcoverage ]
then
    git clone git-adacore:eng/cov/gnatcoverage
fi
if ! [ -d wave ]
then
    anod init wave
fi

# Checkout the right gnatcov branch
cd /Users/itmgr/gnatcoverage
git fetch origin
git checkout origin/$1

# Build gnatcov
cd /Users/itmgr/wave
anod vcs --reset
anod vcs --add-repo gnatcoverage /Users/itmgr/gnatcoverage
anod build gnatcov --smart
anod install gnatall -Qstable
eval $(anod printenv gnatcov)
eval $(anod printenv gnatall -Qstable)

# Then, retrieve the sources of the arch-mix test
SRC_DIR=/Users/itmgr/gnatcoverage/testsuite/tests/U204-026-arch-mix
BUILD_DIR=/cygdrive/c/tmp/U204-026-arch-mix
rm -rf "$SRC_DIR/gen/x86_64-windows"
rsync -ar "$SRC_DIR/" "$BUILD_DIR"
cd "$BUILD_DIR"
chmod +x gen.sh
gnatcov setup
./gen.sh windows

# Copy back the generated sources
rsync -ar "$BUILD_DIR/gen/" "$SRC_DIR/gen"
