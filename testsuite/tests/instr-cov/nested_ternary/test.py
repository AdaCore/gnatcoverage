"""
Check that the instrumentation of a nested ternary expression works. It used
to produce duplicate SCOs and make gnatcov crash.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

# Build and produce a coverage report for the test project.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel='stmt+decision',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
)

check_xcov_reports('*.xcov', {'main.c.xcov': {'!': {4}}}, "xcov")

thistest.result()
