"""
Check that units whose compilation unit contains pragmas after the body are
properly handled. They used to make the instrumenter crash.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
)
check_xcov_reports('xcov/*.xcov', {
    'xcov/main.adb.xcov': {'+': {6}},
    'xcov/raise_error.adb.xcov': {'+': {3}},
    'xcov/raise_error.ads.xcov': {},
})

thistest.result()
