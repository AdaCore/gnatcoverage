"""
Check that scan-decisions reports decisions with !tree BDDs.
"""

from e3.diff import diff

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, xcov


Wdir("tmp_")
gprbuild(gprfor(srcdirs=[".."], mains=["p.adb"]))

xcov(["scan-decisions", "--scos=obj/p.ali"], out="out-alis")
dif = diff("../expected.out", "out-alis")
thistest.fail_if(dif, "out-alis != expected regarding scan-decisions:\n" + dif)

xcov(["scan-decisions", "-Pgen.gpr"], out="out-gpr")
dif = diff("../expected.out", "out-gpr")
thistest.fail_if(dif, "out-gpr != expected regarding scan-decisions:\n" + dif)

thistest.result()
