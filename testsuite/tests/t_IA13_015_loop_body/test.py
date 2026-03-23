from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
    xrun,
)


Wdir("tmp_")
gprbuild(gprfor(["for_loop.adb"], srcdirs="../src"), gargs="-O0")
xrun(exepath_to("for_loop"))
xcov("coverage --level=branch --annotate=xcov " + tracename_for("for_loop"))

# We expect the for loop body to be uncovered
thistest.fail_if(
    not match(r"[0-9]+ -:.*X := X \+ 1", "for_loop.adb.xcov"),
    "expect loop body to be uncovered",
)
thistest.result()
