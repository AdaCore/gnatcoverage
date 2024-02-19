"""
Check that the subprograms of interest mechanism works as expected on a C
example. It also acts as a regression testcase as gnatcov used to crash when
instrumenting a source including multiple headers, with at last one of them
not having any coverage obligation.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

cov_args = build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(srcdirs=os.path.join("..", "src"), mains=["main.c"])
    ),
    covlevel="stmt",
    mains=["main"],
    trace_mode='src',
    extra_coverage_args=[
        "-axcov",
        "--subprograms",
        f"{os.path.join('..', 'src', 'pkg.c')}:1",
    ],
)

check_xcov_reports(
    "obj",
    {
        "main.c.xcov": {},
        "bar.h.xcov": {},
        "pkg.c.xcov": {"+": {4}},
    },
)

thistest.result()
