"""
Check that gnatcov correctly processes expression functions that are primitives
of tagged types for MC/DC, e.g. it warns about them, and they are reported as
uninstrumented in the coverage report.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+uc_mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    tolerate_instrument_messages=(
        "cannot instrument an expression function which"
    ),
)

check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {6}},
        "pak.ads.xcov": {
            "+": {4, 5, 6, 13, 14, 15},
            "-": {8, 10},
            "?": {11, 16},
        },
    },
)

thistest.result()
