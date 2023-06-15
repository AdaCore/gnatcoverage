"""
This test checks that coverage obligations in a header files are processed. It
also checks that running gnatcov instrument when the source files are in the
current directory works. gnatcov instrument used to ignore source files because
the sloc of nodes returned by libclang in that case is relative (./pkg.h
instead of <abspath_to_test_dir>/pkg.h).
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import thistest, gprfor


wd = Wdir("tmp_")
for src in ("pkg.h", "pkg.c", "test.c"):
    cp(os.path.join("..", src), src)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=['.'], mains=['test.c'])),
    covlevel='stmt+mcdc',
    mains=['test'],
    extra_coverage_args=['--annotate=xcov', '--output-dir=xcov'],
)

check_xcov_reports(
    '*.xcov',
    {
        'pkg.h.xcov': {'+': {7, 8}, '!': {6}},
        'pkg.c.xcov': {'!': {4}},
        'test.c.xcov': {'+': {6, 7, 8}}
    },
    'xcov')

thistest.result()
