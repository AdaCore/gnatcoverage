#! /bin/sh

# Script to generate SID and traces.

set -ex

cwd=$PWD
cd ..
rsync -ar t_207_srctrace_version/ /tmp/t_207_srctrace_version
cd /tmp/t_207_srctrace_version
./gen.sh
cd $cwd
rsync -ar /tmp/t_207_srctrace_version/ .
