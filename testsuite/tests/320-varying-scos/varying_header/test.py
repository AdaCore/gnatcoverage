from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../src"], mains=["main.c"]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "bar.c.xcov": {},
        "foo.c.xcov": {},
        "foo.h.xcov": {"-": {4, 11}},
        "main.c.xcov": {"+": {4}},
    },
    discard_empty=False,
)

thistest.result()
