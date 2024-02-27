"""
This test checks that dumping traces with errno set to a non-zero value
works. It can be the case in user code if a system call fails.

The source traces dump used to crash as we were checking the errno value
to check for an I/O error when writing the trace file (instead of checking
that we had a valid file descriptor for instance).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(prjid="p", srcdirs=[".."], mains=["main.c"], langs=["C"])
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
)

check_xcov_reports("obj", {"main.c.xcov": {"+": {6, 7}}})
thistest.result()
