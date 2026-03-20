"""
Regression test: the relocation logic for non instrumented SCOs when loading
SCOs/checkpoints contained a logic error, resulting in trying to get the
previous element from a No_Element cursor, resulting in a constraint error.
This only happened if the last SCO of a checkpoint was not instrumented, and
that SCO had already been loaded through another checkpoint.

In this test we use an expression function which is a primitive of a tagged
type, and that type is its return type, to create non-instrumented SCOs.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

gpr = gprfor(mains=["test_f.adb", "test_t.adb"], srcdirs=[".."])


xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gpr, units=["pkg"]),
    extra_coverage_args=[],
    covlevel="stmt",
    mains=["test_f", "test_t"],
    trace_mode="src",
    tolerate_instrument_messages=(
        "gnatcov limitation: cannot instrument an expression function"
    ),
)

trace_t = xcov_args.pop()
trace_f = xcov_args.pop()

# Create a checkpoint from each trace
xcov(xcov_args + ["--save-checkpoint=test_t.ckpt", trace_t])
xcov(xcov_args + ["--save-checkpoint=test_f.ckpt", trace_f])

# Try to consolidate the checkpoint
xcov(
    xcov_args + ["-Ctest_t.ckpt", "-Ctest_f.ckpt", "-axcov"],
    tolerate_messages=(
        "warning: Specifying units of interest through --units"
        " has no effect on checkpoints"
    ),
)

check_xcov_reports("obj", {"pkg.ads.xcov": {"?": {10}, "+": {5, 6, 7}}})

thistest.result()
