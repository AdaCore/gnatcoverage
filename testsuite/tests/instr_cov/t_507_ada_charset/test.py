"""
Test that gnatcov correctly instruments Ada source files with UTF-8
identifiers.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs="..",
            mains=["main.adb"],
            compiler_extra="""
                for Default_Switches ("Ada") use ("-gnatW8");
            """,
        )
    ),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {4, 6}},
        "pkg.adb.xcov": {"!": {9}},
        "pkg.ads.xcov": {},
    },
    discard_empty=False,
)

thistest.result()
