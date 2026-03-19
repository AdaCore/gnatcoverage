"""
Test that using manual dump and automatic dump at the same time creates 2
source traces as expected.
"""

from SCOV.minicheck import build_and_run, check_xcov_content, xcov
from SCOV.instr import default_dump_channel
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import exename_for, gprfor

tmp = Wdir("tmp_")

p = gprfor(
    mains=["main.adb"],
    srcdirs=["../src"],
)

cov_args = build_and_run(
    gprsw=GPRswitches(root_project=p, units=["main"]),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    extra_instr_args=["--dump-filename-simple"],
    dump_trigger=["manual", "main-end"],
    manual_prj_name="gen",
    split_extracted=True,
)

if default_dump_channel() == "base64-stdout":
    # The instrumented programs only print base64 traces on the standard
    # output. It is build_and_run that runs the "gnatcov extract-base64-trace"
    # command to generate separate traces, and that assigns them the "gen-main"
    # name prefix.
    manual_trace = "gen-main.srctrace"
    auto_trace = "gen-main-1.srctrace"
else:
    # The name of the trace created by the manual indication is determined by
    # the prefix given in the pragma: "manual_dump".
    manual_trace = "manual_dump.srctrace"
    # The name of the trace created by the "main-end" trigger uses the
    # executable name as the prefix (including the potential file extention:
    # ".exe" on Windows).
    auto_trace = exename_for("main") + ".srctrace"


thistest.comment("Check the manual report")
xcov(
    [
        "coverage",
        "--level=stmt",
        "-Pgen",
        "-axcov",
        manual_trace,
        "--output-dir",
        "manual_report",
    ]
)
check_xcov_content(
    "manual_report/main.adb.xcov",
    {
        "+": {9, 11},
        "-": {6, 12, 17, 18, 19},
    },
)

thistest.comment("Check the automatic report")
xcov(
    [
        "coverage",
        "--level=stmt",
        "-Pgen",
        "-axcov",
        auto_trace,
        "--output-dir",
        "auto_report",
    ]
)
check_xcov_content(
    "auto_report/main.adb.xcov",
    {
        "+": {6, 17, 18, 19},
        "-": {9, 11, 12},
    },
)

thistest.comment("Check the merge of both traces")
xcov(["coverage", "--level=stmt", "-Pgen", "-axcov", auto_trace, manual_trace])
check_xcov_content(
    "obj/main.adb.xcov",
    {
        "+": {6, 9, 11, 17, 18, 19},
        "-": {12},
    },
)


thistest.result()
