"""
Check that gnatcov correctly detects annotation inconsistencies, e.g. when the
user tries to produce a report from a source trace resulting from the
instrumentation of a program with annotations that are not the same as the
current source instrumentation data.
"""

import os
import os.path

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov

Wdir("tmp_")

# Generate the root project
root_project = gprfor(srcdirs=[os.path.join("..")], mains=["test.c"])
gprsw = GPRswitches(root_project)

# Configure the build with -DA which results in the inclusion of an annotated
# region at pkg.c from line 3 to line 6.
tmp_a = Wdir("a")
trace_a = os.path.abspath(
    build_and_run(
        gprsw=gprsw,
        covlevel="stmt",
        mains=["test"],
        extra_instr_args=["--c-opts=-DA"],
        gpr_exe_dir="..",
        extra_coverage_args=[],
        trace_mode="src",
    )[-1]
)
tmp_a.to_homedir()

# Configure the build with -DB which results in the inclusion of an annotated
# region at pkg.c from line 8 to line 11, thus differing from the above.
tmp_b = Wdir("b")
trace_b = os.path.abspath(
    build_and_run(
        gprsw=gprsw,
        covlevel="stmt",
        mains=["test"],
        extra_instr_args=["--c-opts=-DB"],
        trace_mode="src",
        gpr_exe_dir="..",
        extra_coverage_args=[],
    )[-1]
)
tmp_b.to_homedir()

# Now, try to generate a coverage report from both traces
os.mkdir("xcov")
xcov(
    ["coverage"]
    + gprsw.cov_switches
    + ["-axcov", "--output-dir=xcov", "--level=stmt", trace_a, trace_b],
    tolerate_messages="traces for .* are inconsistent",
    out="cov.out",
)

# Check that gnatcov warns about inconsistency between a.srctrace and the
# source instrumentation data.
thistest.fail_if_no_match(
    "'gnatcov coverage' output",
    r"warning: traces for .* are inconsistent with the corresponding Source"
    " Instrumentation Data",
    contents_of("cov.out"),
)

# Check that the coverage report is as expected (e.g. a.srctrace is skipped).
check_xcov_reports(
    "xcov",
    {
        "test.c.xcov": {"+": {6, 7}},
        "pkg.c.xcov": {"D": {10, 11, 12}, "-": {14}},
    },
)

thistest.result()
