"""
Check that the instrumentation of a project mixing C and C++ works. It used to
produce files with the same object name, resulting in an error at build time.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["test.cpp"])),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=["--annotate=xcov"],
)

check_xcov_reports(
    "obj",
    {
        "test.cpp.xcov": {"+": {6, 7}},
        "pkg.c.xcov": {"+": {4}},
    },
)

thistest.result()
