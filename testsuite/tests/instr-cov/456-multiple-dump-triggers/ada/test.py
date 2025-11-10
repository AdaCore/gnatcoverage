"""
Test that using manual dump and automatic dump at the same time creates 2
source traces as expected.
"""

from SCOV.minicheck import build_and_run, check_xcov_content, xcov
from SCOV.instr import default_dump_channel
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

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

# When traces are dumped with base 64, the trace names are generated
# automatically by the testsuite
(manual_trace, auto_trace) = (
    ("gen-main.srctrace", "gen-main-1.srctrace")
    if default_dump_channel() == "base64-stdout"
    else ("manual_dump.srctrace", "main.srctrace")
)


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
