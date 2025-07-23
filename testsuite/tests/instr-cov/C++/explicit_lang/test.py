"""
Check that gnatcov explicitly passes the language to the compiler / parsing
command, otherwise the file might be preprocessed as C code instead of C++
and reciprocally.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

p = gprfor(
    srcdirs=[".."],
    mains=["main.c"],
    langs=["C++"],
    extra="""
       package Naming is
           for Body_Suffix ("C++") use ".c";
       end Naming;
    """,
)
# Build and produce a coverage report for the test project.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports("xcov", {"main.c.xcov": {"+": {7, 14, 15}}})

thistest.result()
