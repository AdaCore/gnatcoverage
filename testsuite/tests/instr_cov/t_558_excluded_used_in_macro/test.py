"""
Regression testcase checking that gnatcov correctly remaps source file
indexes identifying ignored sources in macro expansions. When built in
dev-mode, gnatcov used to crash with an assertion failure. When built in prod,
the offending source file index was not remapped and thus wrong.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.c"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "--excluded-source-files=pkg.h",
        "-axcov+",
        "--output-dir=xcov",
    ],
    trace_mode="src",
)

thistest.fail_if(
    "note: in definition of macro PRINT_HW at location pkg.h:3:18"
    not in contents_of("xcov/main.c.xcov")
)

thistest.result()
