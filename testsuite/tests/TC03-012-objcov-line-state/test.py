"""
Testcase for TC03-012: Check that line coverage state is correctly computed,
even if some debug information is missing from the executable.
"""

import os
from e3.os.process import Run

from SCOV.minicheck import check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import exepath_to, exename_for, gprfor, xcov, xrun, tracename_for

Wdir('tmp')

prj = gprfor(srcdirs=['..'], objdir='.', mains=['main.c'], prjid='gen')

# Compile assembly
Run(['gcc', '-o', exename_for('main'), os.path.join('..', 'main.s')])

# Run and coverage
xrun(['-cinsn', exepath_to('main')])
xcov(['coverage', '-cinsn', '-axcov', '-Pgen', tracename_for('main')])

# Check that line 3 is correctly marked as partially covered
check_xcov_reports('.', {'main.c.xcov': {'-': {4}, '!': {3}}})

thistest.result()
