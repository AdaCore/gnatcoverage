"""
Check that ignoring a C file file works as expected. It used to make "gnatcov
coverage" crash.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["test.c"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=["--annotate=xcov", "--ignore-source-files=foo.c"],
)

check_xcov_reports(
    "*.xcov",
    {
        "test.c.xcov": {"+": {6, 7}}
    },
    "obj",
)

thistest.result()
