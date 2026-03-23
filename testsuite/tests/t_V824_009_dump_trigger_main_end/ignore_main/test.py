"""
Check that gnatcov --dump-trigger=main-end produces valid code when the main
is not a unit of interest and thus when the main code was not curlified.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["test.c"], srcdirs=[".."]), units=["foo.c"]
    ),
    covlevel="stmt+mcdc",
    mains=["test"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    dump_trigger="main-end",
)

check_xcov_reports("xcov", {"foo.c.xcov": {"+": {4}}})

thistest.result()
