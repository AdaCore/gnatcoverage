"""
Check that custom naming conventions for file-based languages are properly
handled.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        # gprfor assumes standard naming convention for mains, so we must
        # control the mains manually.
        root_project=gprfor(
            srcdirs=[".."],
            mains=[],
            langs=["C++"],
            extra="""
                for Main use ("main.cc");
                package Naming is
                   for Body_Suffix ("C++") use ".cc";
                   for Spec_Suffix ("C++") use ".hh";
                end Naming;
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
        "main.cc.xcov": {"+": {11, 12}},
        "hello.cc.xcov": {"+": {6}},
    },
)

thistest.result()
