"""
Test that the instrumentation of C sources with filenames that contain uncommon
characters produces valid instrumented sources.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=['..'], mains=['main.c', 'ada_main.adb'])
    ),
    covlevel='stmt',
    mains=['main', 'ada_main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
)
check_xcov_reports(
    '*.xcov',
    {
        'main.c.xcov': {'+': {6}},
        '$foo@bar$.c.xcov': {'+': {4}},
        'ada_main.adb.xcov': {'+': {9}},
    },
    'xcov',
)

thistest.result()
