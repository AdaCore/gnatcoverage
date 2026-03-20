"""
Regression testcase: when using the manual dump-trigger, we used to have
mismatching coverage obligations between the step recording preprocessing
information and the step instrumenting. That was because we were missing
standard preprocessor search paths when parsing the source file in the former
step

As a reminder, to record preprocessing information, we parse the unpreprocessed
file with clang, and we inhibit builtin macros, as well as standard compiler
headers to fully mimick the behavior of the preprocessor _in use_ (e.g. gcc).
This means that if we are missing part of that configuration, which was the
case there, we end up with an ill-formed AST. This results in wrong coverage
obligations for the step recording preprocessing information.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name="gen",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports("xcov", {"main.c.xcov": {"+": {6}, "-": {8}}})

thistest.result()
