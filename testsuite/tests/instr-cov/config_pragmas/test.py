"""
Check that configuration pragmas are correctly interpreted by the instrumenter.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(
            srcdirs=[".."],
            mains=["main.adb"],
            compiler_extra=(
                'for Local_Configuration_Pragmas use "../gnat.adc";'
            ),
        )
    ),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)

# If for some reason the instrumenter ignores the Pure_Barrier restriction in
# gnat.adc, it will instrument the entry barrier decision "when Cond" in
# pkg.adb, which will then not compile because the witness call inserted for
# that decision violates the Pure_Barrier restriction.
check_xcov_reports(
    "*.xcov",
    {
        "main.adb.xcov": {"+": {4, 6, 8, 10}},
        "pkg.ads.xcov": {"+": {6}},
        "pkg.adb.xcov": {"+": {12}, "-": {23}, "?": {20}},
    },
    cwd="xcov",
)

thistest.result()
