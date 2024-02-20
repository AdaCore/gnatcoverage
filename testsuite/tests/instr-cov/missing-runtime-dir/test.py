"""
Check that instrumentation an Ada project does not fail when the
ADA_INCLUDE_PATH environment variable (classified as a "runtime directory" by
the GPR system) does not exist.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


Wdir("tmp_")

os.environ["ADA_INCLUDE_PATH"] = os.path.abspath("nosuchdir")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
)
check_xcov_reports("obj", {"main.adb.xcov": {"+": {5}}})

thistest.result()
