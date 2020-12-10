"""
Check that invalid units passed as project attributes are properly reported.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches, gprcov_for
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor


tmp = Wdir('wd_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(
        'main.adb',
        srcdirs='..',
        extra=gprcov_for(units_in=['no_such_unit', 'main']))),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov'])

log_file = ('coverage.log'
            if thistest.options.trace_mode == 'bin' else
            'instrument.log')
thistest.fail_if_not_equal(
    'gnatcov output',
    'warning: no unit no_such_unit in project gen (coverage.units attribute)',
    contents_of(log_file).strip())

check_xcov_reports('obj/*.xcov', {'obj/main.adb.xcov': {'+': {5}}})

thistest.result()
