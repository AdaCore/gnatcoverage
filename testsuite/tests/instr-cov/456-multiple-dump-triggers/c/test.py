"""
Test that using manual dump and automatic dump at the same time creates 2
source traces as expected.
"""

from SCOV.minicheck import build_and_run, check_xcov_content, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

p = gprfor(
    mains=["main.c"],
    srcdirs=["../src"],
)

cov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    extra_instr_args=["--dump-filename-simple"],
    dump_trigger=["manual", "main-end"],
    manual_prj_name="gen",
)

thistest.comment("Check the manual report")
xcov(
    [
        "coverage",
        "--level=stmt",
        "-Pgen",
        "-axcov",
        "manual_dump.srctrace",
        "--output-dir",
        "manual_report",
    ]
)
check_xcov_content(
    "manual_report/main.c.xcov",
    {
        "+": {10, 12},
        "-": {4, 14, 20, 21, 22, 24},
    },
)

thistest.comment("Check the automatic report")
xcov(
    [
        "coverage",
        "--level=stmt",
        "-Pgen",
        "-axcov",
        "main.srctrace",
        "--output-dir",
        "auto_report",
    ]
)
check_xcov_content(
    "auto_report/main.c.xcov",
    {
        "+": {4, 20, 21, 22, 24},
        "-": {10, 12, 14},
    },
)

thistest.comment("Check the merge of both traces")
xcov(
    [
        "coverage",
        "--level=stmt",
        "-Pgen",
        "-axcov",
        "main.srctrace",
        "manual_dump.srctrace",
    ]
)
check_xcov_content(
    "obj/main.c.xcov",
    {
        "+": {4, 10, 12, 20, 21, 22, 24},
        "-": {14},
    },
)


thistest.result()
