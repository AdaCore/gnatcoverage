"""
Check that when producing multiple reports in one "gnatcov coverage"
execution messages are not emitted multiple times.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Test project to instrument
prj = gprfor(srcdirs=[".."], mains=["main.adb"], objdir="obj")

# Generate multiple reports for prj in a single "gnatcov coverage" execution
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-areport,xcov", "--output-dir=obj"],
    out="coverage.log",
)

# Check the expected results
expected_cov = {"main.adb.xcov": {}, "pkg.adb.xcov": {"-": {10}}}
if thistest.options.trace_mode == "src":
    expected_cov["pkg.ads.xcov"] = {}
check_xcov_reports("obj", expected_cov, discard_empty=False)

# Check that we do not have any duplicate messages. In this case, we only
# expect a single statment violation.
thistest.fail_if_no_match(
    "More than one violation detected",
    r"(.|\n)*2.1. STMT COVERAGE"
    r"\n------------------"
    r"\n"
    r"\npkg.adb:10:7: statement not executed"
    r"\n"
    r"\n1 violation.(.|\n)*",
    contents_of("coverage.log"),
)

thistest.result()
