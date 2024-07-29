"""
Check that instrumenting a project that contains an "orphan" unit (i.e. a
source file present in the project but not used in the build and not
compilable) works as expected.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

expected_cov = {"main.adb.xcov": {"+": {5}}}
if thistest.options.trace_mode == "src":
    expected_cov["pkg-child.ads.xcov"] = {}
    expected_cov["pkg-child.adb.xcov"] = {"-": {6}}

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    tolerate_instrument_messages=".",
    extra_coverage_args=["--annotate=xcov"],
    tolerate_coverage_messages="no ALI file found for unit pkg.child",
)

if thistest.options.trace_mode == "src":
    thistest.fail_if_not_equal(
        '"gnatcov instrument" output',
        (
            "warning: While instrumenting pkg-child.ads...\n"
            "warning: Cannot find required source file: pkg.ads"
        ),
        contents_of("instrument.log").strip(),
    )

check_xcov_reports("obj", expected_cov)

thistest.result()
