"""
Check that gnatcov does not complain about "same base name for files" and
computes the expected code coverage when C files are compiled with a Makefile.

This used not to work because in this build scenario on Windows, debug info
used to contain unexpected double slash as directory separators.
"""

import os.path

import shutil

from e3.os.process import Run

from SCOV.minicheck import check_xcov_reports
from SUITE.cutils import FatalError
from SUITE.tutils import exepath_to, thistest, tracename_for, xcov, xrun


if os.path.exists('coverage'):
    shutil.rmtree('coverage')
p = Run(['make', 'clean'])

main = exepath_to('driver')
main_trace = tracename_for('driver')

p = Run(['make'])
thistest.stop_if(p.status != 0, FatalError('call to "make" failed'))

xcflags = ['-cstmt+mcdc', '--scos=wibble.c.gli', '--scos=driver.c.gli']

xrun(xcflags + [main])
xcov(['coverage'] + xcflags
     + ['-axcov', '--output-dir=coverage', '-T', main_trace])

check_xcov_reports(
    os.path.join('coverage', 'wibble.c.xcov'),
    {os.path.join('coverage', 'wibble.c.xcov'): {'+': {8, 12}, '!': {6}}}
)

thistest.result()
