"""
Check that gnatcov uses projects files to find SCOs for C units.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, empty, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir('tmp_')

# Create a very simple C project
gpr = gprfor(mains='foo.c', srcdirs=['../src'], langs=["C"])

# By default build_and_run will use -P to designate the units of interest,
# which is what we want here.

coverage_log = 'coverage.log'

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    mains=['foo'],
    covlevel='stmt',
    extra_coverage_args=['-axcov', '--output-dir=.'],
    out=coverage_log
)

thistest.fail_if(
    not empty(coverage_log),
    comment='"gnatcov coverage" output not empty:\n' +
    contents_of(coverage_log)
)

check_xcov_reports('*.xcov', {
    'foo.c.xcov': {'+': {6}},
    'bar.c.xcov': {'+': {6}},
    'bar.h.xcov': {'+': {9}}
}
)

thistest.result()
