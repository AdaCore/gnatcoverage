"""
This test performs a bunch of xcov related operations to validate the basic
toolset functionalities.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    maybe_valgrind,
    tracename_for,
    xcov,
    xrun,
)


Wdir("tmp_")

# Context information, basic command line interface checks
print("maybe_valgrind prepends ...", maybe_valgrind([]))

gprbuild(
    project=gprfor(
        mains=["test_min.adb"], langs=["Ada", "Asm"], srcdirs=[".."]
    )
)
xrun(exepath_to("test_min"))
xcov(
    [
        "coverage",
        "--level=branch",
        "--annotate=asm",
        tracename_for("test_min"),
    ],
    "test_min.out",
)
thistest.fail_if(
    not match(r"min \+:.*", "test_min.out"), "full coverage expected"
)

thistest.result()
