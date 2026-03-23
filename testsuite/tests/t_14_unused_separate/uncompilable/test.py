"""
Check that instrumenting a project that contains an "orphan" subunit (i.e. a
subunit never referenced in its parent unit) that does not compile works as
expected, i.e. does not issue analysis warnings at `gnatcov instrument` time.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            mains=["main.adb"], srcdirs=["../src", "../../src"]
        )
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    tolerate_coverage_messages="no ALI file found for unit pkg.bar",
)

thistest.fail_if(
    os.path.exists("obj/gen-gnatcov-instr/pkg-bar.adb"),
    "separate orphan unit instrumented",
)

expected_cov = {
    "main.adb.xcov": {"+": {5}},
    "pkg.ads.xcov": {"+": {2}},
    "pkg.adb.xcov": {"-": {2}},
}
check_xcov_reports("obj", expected_cov)

thistest.result()
