#! /bin/sh
#
# Script to update SID/checkpoints/traces/... that are baselined in the
# testsuite sources.

set -eu

for test_dir in \
    tests/t_207_srctrace_version \
    tests/t_U204_026_arch_mix \
; do
    echo "##"
    echo "## $test_dir"
    echo "##"
    (
        cd $test_dir
        sh update.sh
    )
done
