"""
Check that gnatcov does not report coverage violations for decisions in
``sizeof``.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
foo_optimized_out = {4} if thistest.options.trace_mode == "bin" else set()
check_xcov_reports(
    "xcov",
    {
        "main.c.xcov": {"+": {8, 9}, "!": {7}},
        "foo.c.xcov": {"+": {4, 10} - foo_optimized_out},
    },
)
thistest.result()
