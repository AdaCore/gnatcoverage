"""
Test that "gnatcov disp-routines" command does not crash with twice the same
program.
"""

import os
import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import exename_for, gprbuild, gprfor, xcov


wd = Wdir('tmp_')

gprbuild(gprfor(srcdirs=[os.path.join(wd.homedir)],
                mains=['foo.adb']))
exename = exename_for('foo')

xcov(['disp-routines',
      '--include', exename,
      '--exclude', exename], out='disp-routines.out')
thistest.fail_if(
    contents_of('disp-routines.out') != '',
    '"gnatcov disp-routines" did not yield the expected list of routines'
)

thistest.result()
