
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor


wd = Wdir('wd_', clean=True)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=['test_lt0.adb'],
                                          srcdirs=['../src'],
                                          deps=['../App/app'])),
    covlevel='stmt',
    mains=['test_lt0'],
    extra_args=['--projects=app_base'],
    extra_coverage_args=['-axcov'])

# App_Base is extended by App; App overrides Coverage'Units so that only Values
# (not Values.Aux) is selected.
check_xcov_reports('obj/*.xcov', {
    'obj/values.ads.xcov': {},
    'obj/values.adb.xcov': {'+': {5, 6}, '-': {8}}})

thistest.result()
