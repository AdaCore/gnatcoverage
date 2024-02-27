"""
Test that there is no issue with C sources that have the same basename.
"""

import os.path

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
    xrun,
)


# This must work for source coverage...
TestCase(category=CAT.stmt).run()

# ... and for object coverage, too!
tmp_ = Wdir("tmp_")

gprfile = gprfor("test_foo.c", srcdirs=os.path.join("..", "src"))
exefile = exepath_to("test_foo")
tracefile = tracename_for("test_foo")
logfile = "xcov-coverage.log"

routinesfile = "routines.list"
with open(routinesfile, "w") as f:
    for symbol in ("main", "foo", "other_foo"):
        f.write("{}\n".format(symbol))

gprbuild(gprfile)
xrun(exefile)

# If this test fail, we expect this is because the following command will emit
# warnings about various source files having the same base name.
xcov(
    [
        "coverage",
        "-P{}".format(gprfile),
        "--level=insn",
        "--annotate=xcov",
        "--routines=@{}".format(routinesfile),
        tracefile,
    ],
    out=logfile,
)

thistest.fail_if(
    os.path.getsize(logfile) > 0,
    "xcov standard output not empty ({}):\n--\n{}".format(
        logfile, contents_of(logfile)
    ),
)

thistest.result()
