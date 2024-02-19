"""
Check that compiler switches are properly imported from project files.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=[".."],
            mains=["main.c"],
            compiler_extra="""
                for Default_Switches ("C") use ("-DA");
                for Switches ("main.c") use ("-DB");
            """,
        ),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov",
    {
        "main.c.xcov": {"+": {16, 19}},
        "hello.c.xcov": {"+": {7}},
    },
)

thistest.result()
