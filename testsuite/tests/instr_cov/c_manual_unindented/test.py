"""
Regression testcase: unindented manual dump/reset directives used to be
incorrectly replaced in instrumented sources due to an off-by-one bug (the
comment opening character used to survive the instrumentation while the rest of
the comment was removed).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")
build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(srcdirs=[".."], mains=["main.c"]), units=["main.c"]
    ),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name="gen",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_instr_args=["--save-temps"],
)
check_xcov_reports("xcov", {"main.c.xcov": {"+": {7}, "-": {9}}})
thistest.result()
