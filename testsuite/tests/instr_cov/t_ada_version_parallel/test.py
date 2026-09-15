"""
Regression test: gnatcov used to ignore the --ada option when instrumenting
with "gnatcov instrument-source", in parallel mode. This test thus checks that
gnatcov instruments a source without a warning, when it features a construct
supported only when instrumenting with Ada 2022.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

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

thistest.result()
