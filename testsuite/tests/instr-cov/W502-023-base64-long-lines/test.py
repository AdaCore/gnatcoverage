"""
Check that "gnatcov extract-base64-trace" handles correctly input files with
very long lines (it used to crash with a stack overflow).
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov


Wdir("tmp_")

# First produce a source trace from the test program
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
    dump_trigger="main-end",
    dump_channel="base64-stdout",
)

# Then produce an alternate base64 trace with very long lines
with open("base64_long_lines.txt", "w") as f:
    for _ in range(10000):
        f.write("A" * 1000)
    f.write("\n")
    f.write(contents_of("main_output.txt"))

# Extract the source trace from base64
xcov(
    ["extract-base64-trace", "base64_long_lines.txt", xcov_args[-1]],
    out="extract.txt",
)
thistest.fail_if_not_equal(
    "'gnatcov extract-base64-trace' output",
    "",
    contents_of("extract.txt"),
)

# Produce the coverage report and check its contents
xcov(xcov_args, out="coverage.txt")
thistest.fail_if_not_equal(
    "'gnatcov coverage' output",
    "",
    contents_of("coverage.txt"),
)
check_xcov_reports("*.xcov", {"main.adb.xcov": {"+": {5}}}, "obj")

thistest.result()
