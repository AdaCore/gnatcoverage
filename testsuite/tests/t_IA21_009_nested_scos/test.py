from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, xcov


Wdir("tmp_")
gprbuild(gprfor(["nested_same_sloc.adb"], srcdirs="../src"))
xcov(["map-routines", "--scos=obj/nested_same_sloc.ali", "nested_same_sloc"])
thistest.result()
