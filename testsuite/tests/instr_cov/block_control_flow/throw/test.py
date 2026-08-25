"""
Regression test: check that block instrumentation ends the current block on a
C++ throw expression, whether it is a statement of its own, nested in an
expression, or nested in the init-capture of a lambda.

The body of a lambda is a separate matter: it runs on call, so a throw there is
a throw from a callee, which does not end the enclosing block. Check that too:
the statements before the call are then reported as uncovered, the false
negative that block instrumentation accepts on exception propagation.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

# Statements of pkg.cpp reached and discharged by their block witness
COVERED = {6, 9, 10, 15, 17, 23, 26, 27, 32, 34, 40, 43, 49, 51, 66, 68}

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=[".."],
            mains=["main.cpp"],
            # Init-captures require C++14
            compiler_extra='for Default_Switches ("C++") use ("-std=c++14");',
        )
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_instr_args=["--instrument-block"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.cpp.xcov": {"+": {6, 7, 8, 9, 10}},
        "pkg.cpp.xcov": {
            "+": COVERED,
            # 44: the lambda is built, never called, so its body is uncovered.
            # 61: the lambda body throws, so only the throw runs there.
            "!": {44, 61},
            # 57 and 60 do run. The throw out of the lambda body leaves them
            # short of their block witness.
            "-": {11, 28, 45, 57, 60, 62},
        },
    },
    discard_empty=False,
)

thistest.result()
