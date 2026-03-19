"""
Check that gnatcov coverage produces a report even when the user did not
specify traces or checkpoints. Also check that the tool warns in this case.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor, xcov

Wdir("tmp_")

cov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
)

# Do not pass the trace to the gnatcov coverage invocation
print("cov args are " + str(cov_args[:-1]))
xcov(cov_args[:-1], tolerate_messages=".*", out="coverage.log")

thistest.fail_if_not_equal(
    '"gnatcov coverage" output',
    "warning: No trace files specified. GNATcoverage will still generate a"
    " report",
    contents_of("coverage.log").strip(),
)

check_xcov_reports("obj", {"main.adb.xcov": {"-": {5}}})

thistest.result()
