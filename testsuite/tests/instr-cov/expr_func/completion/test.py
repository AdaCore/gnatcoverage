"""
Regression test: check that gnatcov does not instrument expression function
that are a primitive of a tagged type T when the controlling parameter is the
return type, and when the expression function is a completion.

gnatcov used to instrument such expression function, which resulted in
introducing a new primitive (the wrapper generated for MC/DC instrumentation),
which was not defined for derived types.
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
        "main.adb.xcov": {"+": {5, 7}},
        "pak.ads.xcov": {"+": {5, 10, 11, 12, 15, 16, 17}, "?": {13, 18}},
    },
)

thistest.result()
