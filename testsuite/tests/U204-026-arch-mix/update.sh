#! /bin/bash

set -ex

cwd=$PWD
unamestr=$(uname)
if [ "$unamestr" != "Linux" ]
then
    echo "Please run this script from a linux OS"
    exit 1
fi

# Update the linux artifacts
cd ..
cp -rf U204-026-arch-mix /tmp/
cd /tmp/U204-026-arch-mix
./gen.sh linux
cd $cwd
cp -rf /tmp/U204-026-arch-mix/* .

# Then update the windows artifacts
[ ! -d '/tmp/iod-dev' ] && git clone git-adacore:eng/shared/iod-dev /tmp/iod-dev
/tmp/iod-dev/create-base.py --base-image x86_64-windows-2019
git_branch=$(git rev-parse --abbrev-ref HEAD)
ssh iod 'bash -s' < update_arch_mix_windows.sh $git_branch
rsync -av iod:/cygdrive/c/tmp/U204-026-arch-mix/gen/* gen/
