"""
Check that the instrumentation of generic null procedures produces valid
code. It used to instantiate generic packages with a generic formal not
available at that point, which is not valid Ada code.

For now we do not know how to instrument generic null procedures, so we
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
        root_project=gprfor(srcdirs=["../src"], mains=["main.adb"]),
        units=["pkg"],
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=."],
    trace_mode="src",
    tolerate_instrument_messages="cannot instrument generic null procedures",
)

# We only expect non-instrumented lines
check_xcov_reports(".", {"pkg.ads.xcov": {"?": {8}}})

# Check that the null procedure is reported as a warning
warning_msg = (
    "gnatcov limitation: cannot instrument generic null procedures."
    " Consider turning it into a regular procedure body."
)
thistest.fail_if_not_equal(
    "'gnatcov instrument' log",
    f"!!! pkg.ads:8:4: {warning_msg}\n",
    contents_of("instrument.log"),
)

thistest.result()
