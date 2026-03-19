"""
Check that the instrumentation of generic expression functions produces valid
code. It used to create non-generic expression functions that could reference
generic formals.

For now we do not know how to instrument generic expression functions, so we
emit a warning for them at instrumentation time and always consider them
non-instrumented.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../src"], mains=["test_main.adb"]),
        units=["pkg"],
    ),
    covlevel="stmt+mcdc",
    mains=["test_main"],
    extra_coverage_args=["-axcov", "--output-dir=."],
    trace_mode="src",
    tolerate_instrument_messages=(
        "cannot instrument generic expression function"
    ),
)

# We only expect non-instrumented lines
check_xcov_reports(".", {"pkg.ads.xcov": {"?": {16, 19}}})

# Check that both expression functions are reported as warnings
warning_msg = (
    "gnatcov limitation: cannot instrument generic expression functions."
    " Consider turning it into a regular function body."
)
thistest.fail_if_not_equal(
    "'gnatcov instrument' log",
    f"??? pkg.ads:15:4: {warning_msg}\n" f"??? pkg.ads:18:4: {warning_msg}\n",
    contents_of("instrument.log"),
)

thistest.result()
