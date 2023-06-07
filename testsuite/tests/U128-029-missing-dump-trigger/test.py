"""
Regression testcase for a gnatcov crash when gnatcov instrument is called
without the dump-trigger option.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov

tmp = Wdir("tmp_")
prj = gprfor(srcdirs=[".."], mains=["main.adb"])
xcov(["instrument", "-P", prj, "--level=stmt"])
thistest.result()
