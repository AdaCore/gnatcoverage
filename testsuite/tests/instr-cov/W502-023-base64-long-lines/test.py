"""
Check that "gnatcov extract-base64-trace" handles correctly input files with
very long lines (it used to crash with a stack overflow).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
    dump_trigger="main-end",
    dump_channel="base64-stdout",
)

check_xcov_reports("*.xcov", {"main.adb.xcov": {"+": {5, 6}}}, "obj")

thistest.result()
