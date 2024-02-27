"""
This test performs a bunch of xcov related operations to validate the basic
toolset functionalities.
"""

import re

from SUITE.context import thistest
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
    xrun,
)
from SUITE.cutils import Wdir, contents_of, match


Wdir("tmp_")

# Context information, basic command line interface checks
gprbuild(
    gprfor(
        mains=["test_min.adb", "test_min1.adb", "test_min2.adb"], srcdirs=".."
    ),
    gargs=["-O1"],
)

xrun(exepath_to("test_min"))

# Check that the test exercising both sides of the 'if' stmt
# get full coverage
xcov(
    "coverage --level=branch --annotate=asm " + tracename_for("test_min"),
    out="test_min.out",
)

thistest.fail_if(
    not match(r"_ada_min \+:.*", "test_min.out"), "full coverage expected"
)

# Seek partial branch coverage for the first test exercising only one side of
# the 'if' stmt. Remember what we found.
xrun(exepath_to("test_min1"))
xcov(
    "coverage -c branch -a asm --routines=_ada_min "
    + tracename_for("test_min1"),
    out="test_min1.out",
)

m = re.search("(>|v): .* (bclr|bge|blt|bnl)", contents_of("test_min1.out"))

thistest.fail_if(not m, "couldn't find expected partially covered branch")

dir1 = m.group(1)
insn1 = m.group(2)

# Seek the other partial branch coverage for the second test exercising only
# the other side of the 'if' stmt.
alt_sign = {">": "v", "v": ">"}

xrun(exepath_to("test_min2"))
xcov(
    "coverage --level=branch --annotate=asm --routines=@../func.list "
    + tracename_for("test_min2"),
    out="test_min2.out",
)
thistest.fail_if(
    not match(alt_sign[dir1] + ": .* " + insn1, "test_min2.out"),
    "couldn't find alternate branch coverage",
)

thistest.result()
