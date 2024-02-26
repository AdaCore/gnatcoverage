"""
Check that gnatcov works fine on projects using non-standard naming schemes.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

p = gprfor(
    mains=['main.adb'], prjid='p', srcdirs=['..'],
    extra="""
        package Naming is
            for Spec_Suffix ("Ada") use ".1.ada";
            for Body_Suffix ("Ada") use ".2.ada";
            for Dot_Replacement use "__";
            for Body ("main") use "main.adb";
        end Naming;
    """
)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=report']
)
check_xcov_reports('report', {
    'main.adb.xcov': {'+': {5}},
    'p.1.ada.xcov': {},
    'p.2.ada.xcov': {'+': {4}},
    'p__q.1.ada.xcov': {'+': {3}},
    'p__q.2.ada.xcov': {'+': {6}},
}, discard_empty=False)

thistest.result()
