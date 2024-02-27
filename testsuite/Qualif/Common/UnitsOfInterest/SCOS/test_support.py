"""
Common functions for the set of tests held in subdirectories of
this one.
"""

from SUITE.context import thistest
from SUITE.tutils import exepath_to, tracename_for, xcov, xrun
from SUITE.cutils import Wdir, contents_of, list_to_tmp

import re
import os


def _run_and_cov(pgm, level, sco_args):
    """
    Execute gnatcov run then gnatcov coverage for the provided
    program `pgm`, expected to be the base name of without extension
    of an executable file. Pass `level` as --level to both commands
    and the provided `sco_args` to gnatcov coverage, and gnatcov run
    if we are doing mcdc.
    """

    level_arg = "--level=%s" % level

    # gnatcov run

    run_args = [exepath_to(pgm), level_arg]
    if "mcdc" in level_arg:
        run_args.extend(sco_args)

    xrun(run_args)

    # gnatcov coverage

    cov_args = [
        "--annotate=xcov",
        "--trace=%s" % tracename_for(os.path.basename(pgm)),
        level_arg,
    ] + sco_args

    xcov(["coverage"] + cov_args)


def _tryone(sco_args, odir):
    """
    Exercise one execution and analysis of the test_ab program
    for stmt+mcdc with the provided `sco_args`, switching to a
    temporary output dir `odir` that we create.
    """

    # Create the output dir and execute the commands from there

    wsd = Wdir(subdir=odir)

    _run_and_cov(pgm="../test_ab", level="stmt+mcdc", sco_args=sco_args)

    # Check report contents

    args = "stmt+mcdc / " + " ".join(sco_args)

    test_xcov = contents_of("test_ab.adb.xcov")
    thistest.fail_if(
        not re.search(r"\+:    Monitor.Diamond", test_xcov),
        "failure on check for %s" % args,
    )

    mon_xcov = contents_of("monitor.adb.xcov")
    thistest.fail_if(
        not re.search(r"\!:.*and then", mon_xcov),
        "failure on Decision monitor check for %s" % args,
    )
    thistest.fail_if(
        not re.search(r"\+:.*Hit := Hit \+ 1;", mon_xcov),
        "failure on Hit monitor check for %s" % args,
    )
    thistest.fail_if(
        not re.search(r"\+:.*Miss := Miss \+ 1;", mon_xcov),
        "failure on Miss monitor check for %s" % args,
    )

    wsd.to_homedir()


def check(test_ali, mon_ali):
    """
    Exercise a sequence of run/coverage commands, providing
    SCOs held in the given ali files (`test_ali` for the test driver
    and `mon_ali` for the Monior unit) through different means,
    mixing --scos=<ali-file> and --scos=@<response-file>.
    """

    # To prevent mixups and facilitate investigation in case
    # of failure, we execute each sequence of commands from within
    # a separate subdir. Adjust the paths to ali files accordingly.
    test_ali = os.path.join("..", test_ali)
    mon_ali = os.path.join("..", mon_ali)

    # Check with response file only
    _tryone(
        sco_args=["--scos=@" + list_to_tmp([test_ali, mon_ali])],
        odir="response",
    )

    # Check with --scos only
    _tryone(sco_args=["--scos=" + f for f in [test_ali, mon_ali]], odir="scos")

    # Check with a mix of a response file and --scos
    _tryone(
        sco_args=["--scos=@" + list_to_tmp([test_ali]), "--scos=" + mon_ali],
        odir="mix",
    )
