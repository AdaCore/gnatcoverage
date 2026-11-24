"""
Check that gnatcov behaves properly in the cases where gnatcov cannot
instrument contracts expressed in pragmas: it emits a warning stating that
the expression cannot be instrumented for ATCC. This concerns preconditions,
postconditions and type invariant pragmas.
"""

from SCOV.minicheck import check_xcov_reports, build_and_run, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")


xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../"], mains=["main.adb"])
    ),
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    covlevel="stmt+decision+atcc",
    register_failure=False,
)

thistest.fail_if_no_match(
    "'gnatcov instrument' output",
    r".*gnatcov limitation: cannot find local declarative part for ATCC.*\n"
    + r".*gnatcov limitation: cannot find local declarative part for ATCC.*\n"
    + r".*gnatcov limitation: cannot find local declarative part for ATCC.*",
    contents_of("instrument.log").strip(),
)

xcov(xcov_args)

check_xcov_reports(
    "xcov",
    {
        "pkg_type_invariant.ads.xcov": {
            "+": {6, 13, 14, 15, 16, 20, 22},
            "?": {17},
        },
        "main.adb.xcov": {"+": {7, 9, 13}, "?": {10, 11}},
    },
)

thistest.result()
