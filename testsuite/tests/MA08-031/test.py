from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import gprfor, gprbuild, xcov


Wdir("tmp_")
gprbuild(gprfor(["f.adb"], srcdirs="../src"), gargs="-c")
xcov(["map-routines", "--scos=obj/f.ali", "obj/f.o"], out="xcov.out")
thistest.fail_if(match("warning:", "xcov.out"), "no labeling warning expected")
thistest.result()
