"""
Check that gnatcov produces correct SCOs (i.e. with no source location nesting)
when there are redundant line markers resulting in nested SCOs.

This can happen with macro expansions mixing user-code and systems-header code
(such as the assert macro). The resulting expansion will have multiple line
markers, with a special flag for the code that comes from system headers.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


wd = Wdir("tmp_")

# Build and produce a coverage report for the test project.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports("xcov", {"main.c.xcov": {"+": {6, 10, 14}}})

thistest.result()
