"""
Check that gnatcov correctly loads and interprets a simple decision SCO.
"""
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

gprbuild(gprfor(srcdirs=["../src"], mains=["p.adb"]))
xrun(exepath_to("p"))

xcov(["map-routines", "--scos=obj/pack.ali"])
xcov(
    [
        "coverage",
        "--level=stmt",
        "--annotate=report",
        "--scos=obj/pack.ali",
        "--report=out",
        tracename_for("p"),
    ]
)
thistest.fail_if(
    not match("pack.adb:7:.* statement not executed", "out"),
    "missing expected s- diagnostic with simple decision sco",
)

thistest.result()
