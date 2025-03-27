"""
Check that gnatcov correctly instruments mains for the automatic dump of
coverage buffers even if these mains are in languages for which coverage is
disabled.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


# First check with a project that contains a C unit (to analyze) driven by an
# Ada main (not to analyze).
tmp = Wdir("tmp_ada-main")
thistest.log("== ada-main ==")
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../ada-main"], mains=["main.adb"]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--restricted-to-languages=C"],
    extra_coverage_args=[
        "-axcov",
        "--output-dir=xcov",
        "--restricted-to-languages=C",
    ],
    trace_mode="src",
)
check_xcov_reports("xcov", {"tested.c.xcov": {"+": {5}, "-": {7}}})
tmp.to_homedir()

# Now check with a project that contains an Ada unit (to analyze) driven by a
# C main (not to analyze).
tmp = Wdir("tmp_c-main")
thistest.log("== c-main ==")
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../c-main"], mains=["main.c"]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--restricted-to-languages=Ada"],
    extra_coverage_args=[
        "-axcov",
        "--output-dir=xcov",
        "--restricted-to-languages=Ada",
    ],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "pkg.adb.xcov": {"+": {10, 22}, "-": {12, 23}},
        "pkg.ads.xcov": {},
    },
    discard_empty=False,
)
tmp.to_homedir()

thistest.result()
