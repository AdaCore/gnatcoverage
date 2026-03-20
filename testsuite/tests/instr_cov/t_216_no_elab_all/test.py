"""
Check that units with the No_Elaboration_Code_All pragma still compile after
instrumentation.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {
            "+": {7, 9, 11, 12},
            "!": {13, 15},
            "-": {14, 16},
        },
        "gen_proc.adb.xcov": {"+": {3}},
        "gen_proc.ads.xcov": {},
        "pkg.adb.xcov": {"+": {14, 15}, "!": {17}},
        "pkg.ads.xcov": {},
        "proc.adb.xcov": {"+": {3}},
        "proc.ads.xcov": {},
    },
    discard_empty=False,
)

thistest.result()
