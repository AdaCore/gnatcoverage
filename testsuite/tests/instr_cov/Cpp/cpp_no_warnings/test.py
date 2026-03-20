"""
Check that the instrumentation produces code that compiles without warnings.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.cpp"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_gprbuild_cargs=["-Werror"],
    trace_mode="src",
)

check_xcov_reports("xcov", {"main.cpp.xcov": {"+": {4}}})

thistest.result()
