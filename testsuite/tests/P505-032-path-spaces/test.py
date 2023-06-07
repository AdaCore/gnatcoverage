"""
This test checks that "gnatcov run" behaves correctly when the main program and
the output file paths passed to it contain whitespaces.

This used not to work with the DynamoRIO adapter on x86-windows.
"""

import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exename_for, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp/with spaces')

gpr_file = gprfor(srcdirs=['../..'], mains=['foo.adb'], objdir='obj')
gprbuild(gpr_file)

foo_exe = os.path.abspath(exename_for('foo'))
foo_trace = os.path.abspath(tracename_for('foo'))

xrun(['-o', foo_trace, foo_exe])
xcov(['coverage', '-P', gpr_file, '-c', 'stmt', '-a', 'xcov', foo_trace])

thistest.fail_if(
    not match(r'4 \+: *Put_Line', os.path.join('obj', 'foo.adb.xcov')),
    'Statement at foo.adb:4 is not covered whereas it should be')
thistest.result()
