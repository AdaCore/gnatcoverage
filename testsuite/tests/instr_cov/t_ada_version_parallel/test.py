"""
Regression test: gnatcov used to ignore the --ada option when instrumenting
with "gnatcov instrument-source", in parallel mode. This test thus checks that
gnatcov instruments a source without a warning, when it features a construct
supported only when instrumenting with Ada 2022.
"""

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

wd = Wdir("tmp_")

# Implicit check that gnatcov instrument output is empty
build_and_run(
    GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs="../src")),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=[],
    extra_instr_args=["--ada", "2022", "--force-parallelism", "-j1"],
    extra_gprbuild_cargs=["-cargs:Ada", "-gnat2022"],
)

thistest.fail_if_match(
    "unexpected gnatcov limitation reported with --ada 2022",
    ".*gnatcov limitation.*",
    contents_of("instrument.log"),
)

#  The same construct is not supported when instrumenting for Ada 2012, so a
#  limitation is expected there. Check that it does reach the user: emitted by
#  an "instrument-source" subprocess, it used to be discarded by the parent
#  process when running in quiet mode.

wd.to_subdir("tmp_ada2012")

xcov_instrument(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs="../src")
    ),
    covlevel="stmt+mcdc",
    extra_args=["--ada", "2012", "--force-parallelism", "-j1"],
    gpr_obj_dir="obj",
    out="instrument.out",
)

thistest.fail_if(
    "gnatcov limitation" not in contents_of("instrument.out"),
    "missing gnatcov limitation in the output of parallel instrumentation",
)

thistest.result()
