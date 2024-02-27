from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, xcov


Wdir("tmp_")

# First build the program
gprbuild(gprfor(mains=["return-ternary.c"], srcdirs=".."))

# Then invoke map-routines so that SCOs are loaded.
xcov(
    [
        "map-routines",
        "--scos=obj/return-ternary.c.gli",
        "obj/return-ternary.o",
    ],
    out="map-routines.txt",
)

# The test fail if we find something like:
#   !!! return-ternary.c:3:10: unexpected SCO nesting in SCO #1: STATEMENT at
#   return-ternary.c:3:3-25, discarding nested SCO
thistest.fail_if(
    any(
        line.startswith("!!!")
        for line in open("map-routines.txt", "r").readlines()
    ),
    "map-routines has found something wrong",
)
thistest.result()
