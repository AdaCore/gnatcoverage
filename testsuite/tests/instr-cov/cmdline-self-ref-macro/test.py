"""
Regression test: gnatcov used to produce spurious coverage obligations for code
coming from self-referencing macros defined on the command line, as it was
expanding them another time when parsing the file. This led to warnings at
instrumentation time, and wrong coverage expectations.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=['..'], mains=['main.c'])),
    covlevel='stmt',
    mains=['main'],
    extra_instr_args=['--c-opts=-Dreturn=printf ("Before return"); return'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
)

check_xcov_reports('xcov/*.xcov', {'xcov/main.c.xcov': {'+': {4}}})

thistest.result()
