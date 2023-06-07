"""
We run a partial test of the simple 'engines' example and expect the analyzer
to report only partial coverage of the relevant subprogram return statement.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')

gprbuild(gprfor(['test_engines.adb'], srcdirs='../src'))

xrun(exepath_to('test_engines'))
xcov(['coverage', '--level=branch', '--annotate=xcov',
      tracename_for('test_engines')])

thistest.fail_if(not match(r' \!:.*return.*Stable_P', 'engines.adb.xcov'))
thistest.result()
