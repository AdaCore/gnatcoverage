"""
Check that "gnatcov instrument" works correctly when run with more than one
job.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=[
        # The formatting of command lines for subprocesses (gnatcov
        # instrument-sources) used to incorrectly handle "string list"
        # switches. As a consequence, while a single positional argument was
        # required (the name of the unit to instrument), multiple arguments
        # were passed instead, and so no unit was instrumented in the end.
        "-j2",
        "--log=LOG1",
        "--log=LOG2",
        "-XVAR1=a",
        "-XVAR2=b",
    ],
    extra_coverage_args=["--annotate=xcov"],
)
check_xcov_reports(
    "obj", {"main.adb.xcov": {"+": {7}}, "utils.c.xcov": {"+": {4}}}
)

thistest.result()
