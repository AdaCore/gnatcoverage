"""
Check that "gnatcov coverage" produces the expected coverage report for
projects that include alternative sources for the same unit. This exercizes
both consolidation using source traces and using checkpoints.
"""

from e3.fs import ls, mkdir, mv, sync_tree

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

p = gprfor(
    srcdirs=[".."],
    mains=["main.adb"],
    extra="""
    type Logging_Backend_Type is ("text_io", "gnat_io");
    Logging_Backend : Logging_Backend_Type := external ("LOGGING_BACKEND");

    package Naming is
       for Body ("logging") use "logging__" & Logging_Backend & ".adb";
    end Naming;
    """,
)

# Create one source trace and one checkpoint per scenarios
traces = []
checkpoints = []
for backend in ["text_io", "gnat_io"]:
    new_checkpoint = f"{backend}.ckpt"
    xcov_args = build_and_run(
        gprsw=GPRswitches(
            root_project=p, xvars=[("LOGGING_BACKEND", backend)]
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["--save-checkpoint", new_checkpoint],
        trace_mode="src",
    )
    xcov(xcov_args, out=f"cov-{backend}.txt")

    orig_trace = xcov_args[-1]
    saved_trace = f"trace-{backend}.srctrace"
    mv(orig_trace, saved_trace)
    traces.append(saved_trace)
    checkpoints.append(new_checkpoint)

    # Save the object directory so that we preserve SID files
    sync_tree("obj", f"obj-{backend}")

# Compute the list of SID files
with open("sids.txt", "w") as f:
    for filename in ls("obj-*/*.sid"):
        print(filename, file=f)

# Compute coverage reports using source traces and checkpoints. Check their
# contents in both cases.
for method, xcov_args in [
    ("srctrace", traces),
    ("checkpoint", [f"--checkpoint={filename}" for filename in checkpoints]),
]:
    thistest.log(f"== {method} ==")
    xcov_dir = f"xcov-{method}"
    mkdir(xcov_dir)
    xcov(
        [
            "coverage",
            "--sid=@sids.txt",
            "--level=stmt",
            "--annotate=xcov",
            "--output-dir",
            xcov_dir,
        ]
        + xcov_args,
        out=f"cov-{method}.txt",
    )
    check_xcov_reports(
        xcov_dir,
        {
            "main.adb.xcov": {"+": {5}},
            "logging.ads.xcov": {},
            "logging__gnat_io.adb.xcov": {
                "+": {7, 8, 9, 14},
                "-": {19},
            },
            "logging__text_io.adb.xcov": {
                "+": {7, 8, 13},
                "-": {18},
            },
        },
        discard_empty=False,
    )

thistest.result()
