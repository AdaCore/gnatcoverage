"""
Regression test: check that gnatcov correctly instruments an expression
function when there are use clauses introduced between the declaration of the
EF and its completion, and when the parameter specifications in the completion
leverage the use clause visibility.

gnatcov used to generate an invalid declaration for the augmented expression
function in that case.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+uc_mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {4, 5}},
        "pkg.ads.xcov": {"+": {6, 12}},
        "pkg2.ads.xcov": {},
        "pkg2-child.ads.xcov": {"+": {2}},
    },
)

thistest.result()
