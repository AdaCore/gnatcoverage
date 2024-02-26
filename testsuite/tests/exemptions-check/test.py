"""
Check that a simple decision SCO is correctly loaded and interpreted by xcov.
"""

from e3.diff import diff

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, xcov


tmp = Wdir("tmp_")

gprbuild(gprfor(["hello.adb"], srcdirs="../src"))

xcov(["map-routines", "--scos=obj/hello.ali", "obj/hello.o"], "out")

dif = diff("../expected", "out")
thistest.fail_if(
    dif, "out != expected on exemption pragma placement rules:\n" + dif
)

thistest.result()
