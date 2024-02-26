"""
Regression test: the relocation logic for non instrumented SCOs when loading
SCOs/checkpoints used to be buggy and some non-instrumented SCOs ended up being
instrumented during "gnatcov coverage".
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

gpr = gprfor(mains=["main.adb"], srcdirs=[".."])

# Instrument the pkg_1 and pkg_2 units. Both contain unsupported constructs
# (generic expression functions), so both will create non-instrumented SCO
# statements.
#
# The coverage step will load SID files for both units, and this step used to
# discard the non-instrumented info for the second SID file loaded because of a
# bug in the SCO_Id relocation mechanism for non-instrumented SCOs.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr, units=["pkg_1", "pkg_2"]),
    extra_coverage_args=["--annotate=xcov"],
    covlevel="stmt+mcdc",
    mains=["main"],
    trace_mode="src",
    tolerate_instrument_messages=(
        "cannot instrument generic expression function"
    ),
)

check_xcov_reports(
    "obj",
    expected_cov={
        "pkg_1.ads.xcov": {"?": {10}, "+": {12}},
        "pkg_2.ads.xcov": {"?": {10}, "+": {12}},
    },
)

thistest.result()
