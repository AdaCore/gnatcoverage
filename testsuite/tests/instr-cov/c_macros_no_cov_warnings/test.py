"""
Check that we have no warnings at coverage time for a format report that
needs to preprocess the file, to emit macro reporting.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=['..'], mains=['main.c'])),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axml'],
    trace_mode='src',
)

# Check that gnatcov coverage did not raise any warning
thistest.fail_if_not_equal(
    "gnatcov coverage output", "", contents_of("coverage.log")
)

thistest.result()
