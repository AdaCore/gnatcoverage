"""
Check that gnatcov does not emit spurious warning about source files with same
basenames.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

p1 = gprfor(prjid="p1", srcdirs=["../src1"], mains=[], objdir="obj-p1")
p2 = gprfor(prjid="p2", srcdirs=["../src2"], mains=[], objdir="obj-p2")
main_gpr = gprfor(
    prjid="main", mains=["main.c"], srcdirs=[".."], deps=["p1", "p2"]
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=main_gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
)
main_optimized_out = {7} if thistest.options.trace_mode == "bin" else set()
src2_foo_optimized_out = {6} if thistest.options.trace_mode == "bin" else set()
check_xcov_reports(
    "obj",
    {
        "main.c.xcov": {"+": {7, 13, 14} - main_optimized_out},
        "src1-foo.c.xcov": {"+": {6}},
        "src2-foo.c.xcov": {"+": {6, 7, 9} - src2_foo_optimized_out},
    },
)

thistest.result()
