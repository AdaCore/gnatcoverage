"""
Check that statements from a unit entirely untested are reported uncovered
regardless of the visibility on the unit object code in the analysis scope.
"""

import re

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import exepath_to, gprfor, xcov


Wdir("tmp_")


# Build App which encloses two units (bump and double), and a unit test for
# just the bump unit. Then run the unit test.
xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=["../src"], mains=["test_bump.adb", "app.adb"]
        )
    ),
    covlevel="stmt",
    mains=["test_bump"],
    extra_coverage_args=["--annotate=report"],
    scos=["obj/bump", "obj/double"],
)


# Do stmt coverage analysis providing scos for all the units and the unit test
# trace. Check that we have diagnostics of absence of coverage on the elements
# part of the untested unit with or without an additional --exec.


def trycov(exec_args):
    p = xcov(xcov_args + exec_args)
    thistest.fail_if(
        not re.search(r"double.adb:\d+:\d+: statement not executed", p.out),
        "Missing stmt violation on double.adb, exec_args: %s" % str(exec_args),
    )


trycov(exec_args=[])

# --exec does not make sense (and is not allowed) in source trace mode
if thistest.options.trace_mode == "bin":
    trycov(exec_args=["--exec=%s" % exepath_to("app")])

thistest.result()
