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
rsync -ar U204-026-arch-mix/ /tmp/U204-026-arch-mix
cd /tmp/U204-026-arch-mix
./gen.sh linux
cd $cwd
rsync -ar /tmp/U204-026-arch-mix/ .

# Then update the Windows artifacts using an IOD machine. Do not create the IOD
# instance if one already exists
IOD_DEV_DIR=/tmp/iod-dev
INSTANCES_FILE="$IOD_DEV_DIR/instances.txt"
if ! [ -d "$IOD_DEV_DIR" ]
then
    git clone git-adacore:eng/shared/iod-dev "$IOD_DEV_DIR"
fi
iod list --instances > "$INSTANCES_FILE" 2>&1
if ! grep "You have some running instance" "$INSTANCES_FILE" \
    > /dev/null
then
    "$IOD_DEV_DIR/create-base.py" --base-image x86_64-windows-2019
fi

git_branch=$(git rev-parse --abbrev-ref HEAD)
ssh iod 'bash -s' < update_arch_mix_windows.sh $git_branch
rsync -av iod:/cygdrive/c/tmp/U204-026-arch-mix/gen/* gen/
