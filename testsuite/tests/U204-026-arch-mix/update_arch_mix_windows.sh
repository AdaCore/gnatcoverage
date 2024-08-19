# Update windows artifacts for U204-026-arch-mix

if [ $# -eq 0 ]; then
  echo "No arguments provided. Please provide the name of the gnatcov branch"
  exit 1
fi

# Create a sandbox
cd /Users/itmgr
git clone git@ssh.gitlab.adacore-it.com:eng/cov/gnatcoverage
anod init wave

# Checkout the right gnatcov branch
cd /Users/itmgr/gnatcoverage
git fetch origin
git checkout origin/$1

# Build gnatcov
cd /Users/itmgr/wave
anod vcs --add-repo gnatcoverage /Users/itmgr/gnatcoverage
anod build gnatcov --interactive never
eval $(anod printenv gnatcov)
eval $(anod printenv stable-gnatall)

# Then, retrieve the sources of the arch-mix test
rm -rf /Users/itmgr/gnatcoverage/testsuite/tests/U204-026-arch-mix/gen/x86_64-windows
cp -r /Users/itmgr/gnatcoverage/testsuite/tests/U204-026-arch-mix /cygdrive/c/tmp
cd /cygdrive/c/tmp/U204-026-arch-mix
chmod +x gen.sh
gnatcov setup
./gen.sh windows

# Copy back the generated sources
rm -rf 
cp -rf /cygdrive/c/tmp/U204-026-arch-mix/ /Users/itmgr/gnatcoverage/testsuite/tests/
