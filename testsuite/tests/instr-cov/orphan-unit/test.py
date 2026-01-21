"""
Check that instrumenting a project that contains an "orphan" unit (i.e. a
source file present in the project but not used in the build and not
compilable) works as expected.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

expected_cov = {"main.adb.xcov": {"+": {5}}}

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    tolerate_instrument_messages=".",
    extra_coverage_args=["--annotate=xcov"],
    tolerate_coverage_messages=r"no (ALI|SID) file found for unit pkg\.child",
)

check_xcov_reports("obj", expected_cov)

thistest.result()
