"""
Check that gnatcov --dump-trigger=main-end actually dump coverage buffers after
the return expression has been executed.
TODO: add a lambda expression and turn this into a C++ test when C++ support
lands in master.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=['test.c'], srcdirs=['..'])),
    covlevel='stmt+mcdc',
    mains=['test'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
    dump_trigger='main-end',
)

check_xcov_reports('xcov', {'test.c.xcov': {'+': {4}, '!': {5}}})

thistest.result()
