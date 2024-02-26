"""
Verify that consolidation works for object-coverage with asm outputs.
"""

import re

from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.tutils import (
    exename_for,
    exepath_to,
    gprbuild,
    gprfor,
    xcov,
    xrun,
    tracename_for,
)
from SUITE.cutils import Wdir


Wdir("tmp_")

# Build two programs using a common "lib" unit, organized so the unit object
# code is placed at different addresses in the two executables.
gpr = gprfor(srcdirs=["../src"], mains=["p1.adb", "p2.adb"])
gprbuild(gpr)

# Check that the lib subprograms are at different addresses
libsym = target_info().to_platform_specific_symbol("lib__adjust")


def address_range_for(symname, bin):
    m = re.search(
        pattern=r"(?P<address_range>\S*-\S*) symbol for %s" % symname,
        string=xcov(["dump-symbols", bin]).out,
    )

    return m.group("address_range")


thistest.fail_if(
    address_range_for(libsym, exename_for("p1"))
    == address_range_for(libsym, exename_for("p2")),
    "address ranges for %s are identical" % libsym,
)

# Execute both programs:
xrun(exepath_to("p1"))
xrun(exepath_to("p2"))

# Do object level coverage analysis. We expect each program to cover the
# lib symbol only partially, and the two combined to cover the lib symbol
# complete.


def check(pgmlist):
    covnote = "!" if len(pgmlist) == 1 else "+"

    out = xcov(
        "coverage --level=branch --annotate=asm %s --routines=%s"
        % (" ".join(tracename_for(pgm) for pgm in pgmlist), libsym)
    ).out

    results = re.findall(string=out, pattern="%s \\%c:" % (libsym, covnote))
    thistest.fail_if(
        len(results) != 1,
        "unexpected #matches for %s:\n%s" % (pgmlist, results),
    )


check(pgmlist=["p1"])
check(pgmlist=["p2"])
check(pgmlist=["p1", "p2"])

thistest.result()
