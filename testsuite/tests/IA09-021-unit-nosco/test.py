"""
Check that gnatcov doesn't crash while processing the ALI file for
a unit producing not a single SCO.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')
gprbuild(gprfor(['test_assert.adb'], srcdirs='../src'))
xrun(exepath_to('test_assert'))
xcov(['coverage', '--level=stmt', '--annotate=xcov',
      '--scos=obj/checks.ali', tracename_for('test_assert')])
thistest.result()
