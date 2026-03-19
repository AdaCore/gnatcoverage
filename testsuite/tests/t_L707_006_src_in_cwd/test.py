from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
    xrun,
)


Wdir("tmp_")

gprbuild(gprfor(["test_engines.adb"], srcdirs=".."))
xrun(exepath_to("test_engines"))
xcov(
    [
        "coverage",
        "--level=stmt",
        "--annotate=xcov",
        tracename_for("test_engines"),
        "--scos=obj/engines.ali",
    ]
)
thistest.result()
