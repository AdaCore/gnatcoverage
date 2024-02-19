"""
Check that the "non-instrumented SCO" state does not propagate from one
instrumented unit to the other (this is a regression testcase).

The sources are carefully arranged so that we have two units (Pkg_1 and Pkg_2)
that yield equivalent sequences of SCOs (stmt, stmt, decision, condition, stmt,
... see the comments in the Ada sources). In Pkg_1, one decision is not
instrumented (because of the entry body guard limitation under the
Simple_Barriers restriction) while the corresponding one in Pkg_2 is
instrumented, and conversely, so that we check the absence of propagation
whether Pkg_1 is instrumented before Pkg_2 or the opposite.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=[".."], mains=["main.adb"]),
        units=["pkg_1", "pkg_2"],
    ),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "pkg_1.adb.xcov": {"+": {8, 17, 26, 27, 29}, "?": {15}},
        "pkg_1.ads.xcov": {},
        "pkg_2.adb.xcov": {"+": {12, 13, 15, 24}, "?": {22}},
        "pkg_2.ads.xcov": {},
    },
)

thistest.result()
